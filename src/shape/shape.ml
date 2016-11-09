module PG = Program_graph
module G = Graph
module E = Expr
module LS = Lang_state

module Set = Core.Std.Set.Poly
module CZ = Clauses_to_z3
module C = Context

open Z3_type

let ctx = CZ.ctx

let translate graph =
  let label_edges =
    let count = ref (-1) in
    Graph.map_edges (fun e ->
        count := !count + 1;
        (!count, e)) in

  let union_map_list f xs = Set.union_list (List.map f xs) in
  let r_union_map_list f xs = List.fold_left E.R_set.union E.R_set.empty (List.map f xs) in

  let rec expr extra_vars rels t1 t2 e =
    fun (v1 : Z3.Expr.expr) (v2 : Z3.Expr.expr) ->
      let assignments = List.map
          (fun v -> CZ.bi_op E.Eq ctx (assign v t1 v1) (assign v t2 v2)) extra_vars in

      let special = function
        | E.Var (name, _) -> Some (if has_acsor name t1
                                   then Z3.Expr.mk_app ctx (get_acsor name t1) [v1]
                                   else Z3.Expr.mk_app ctx (get_acsor name t2) [v2])
        | _ -> None in

      let e' = CZ.expr special rels [] e in
      CZ.many_op E.And ctx (assignments @ [e'])
  in

  let graph = Field.modify_uses graph in

  let exprs = G.edges graph |> List.map
      (fun (_, _, e) -> match e with
        | PG.Assert (e, at) -> e
        | PG.Body e -> e) in

  let rels = r_union_map_list E.rels exprs |> E.R_set.elements in
  let queries = union_map_list E.queries exprs |> Set.to_list in

  let rels' =
    List.map (fun (lbl, es) -> CZ.relation ("r_" ^ string_of_int lbl, es)) rels
    @
    List.map (fun (lbl, e, at) -> CZ.relation ("q_" ^ string_of_int lbl, [e])) queries in

  let graph = label_edges graph in
  let labels =
    graph
    |> G.map_edges fst
    |> G.map_nodes (fun (l, _) -> l) in

  let assertion = ref None in
  let conn ((lbl1, vs1), (lbl2, vs2), (label, (reads, writes, e))) =
    let extra_vars = Set.inter (Set.of_list vs1) (Set.of_list vs2) |> Set.to_list in

    let (s1, t1) = create_datatype lbl1 vs1 in
    let (s2, t2) = create_datatype lbl2 vs2 in

    match e with
    | PG.Assert (e, at) ->
      let arbitrary = Z3.Arithmetic.Integer.mk_numeral_i ctx 0 in
      assertion := Some ((fun v1 -> expr extra_vars rels' t1 t2 e v1 arbitrary), label);
      { C.label = label
      ; C.predecessors = G.entrances labels lbl1
      ; C.result_sort = s2
      ; C.reads  = []
      ; C.writes = []
      ; C.transition = fun _ _ -> Z3.Boolean.mk_true ctx
      }
    | PG.Body e ->
      let e' = expr extra_vars rels' t1 t2 e in
      { C.label = label
      ; C.predecessors = G.entrances labels lbl1
      ; C.result_sort = s2
      ; C.reads  = List.map (Field.map_desc (fun r -> assign r t2)) reads
      ; C.writes = List.map (Field.map_desc (fun w -> assign w t2)) writes
      ; C.transition = e'
      }
  in
  let contexts = List.map conn (G.conns graph) in
  (contexts, Option.get !assertion)

let derive classpath class_name =
  Inspect.inspect classpath class_name
  |> translate
