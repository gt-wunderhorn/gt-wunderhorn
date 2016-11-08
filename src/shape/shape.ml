module PG = Program_graph
module G = Graph
module E = Expr
module LS = Lang_state

module Set = Core.Std.Set.Poly

module CZ = Clauses_to_z3

open Z3_type

type zexpr = Z3.Expr.expr
type bb = PG.lbl
type field = string
type write = PG.lbl * field * E.var * E.var
type state =
  { graph  : (PG.lbl * Z3.Sort.sort, zexpr -> zexpr -> zexpr) G.t
  ; writes : write list
  }


let ctx = CZ.ctx

(** We leverage the normal expression handling procedure except that variables
    are converted to a function on Z3 expressions. *)
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

let union_map_list f xs = Set.union_list (List.map f xs)
let r_union_map_list f xs = List.fold_left E.R_set.union E.R_set.empty (List.map f xs)

let translate graph =
  let exprs = G.edges graph in

  let rels = r_union_map_list E.rels exprs |> E.R_set.elements in
  let queries = union_map_list E.queries exprs |> Set.to_list in

  let rels' =
    List.map (fun (lbl, es) -> CZ.relation ("r_" ^ string_of_int lbl, es)) rels
    @
    List.map (fun (lbl, e, at) -> CZ.relation ("q_" ^ string_of_int lbl, [e])) queries in

  let conn ((lbl1, vs1, s1, t1), (lbl2, vs2, s2, t2), e) =
    let extra_vars =
      Set.inter (Set.of_list vs1) (Set.of_list vs2) |> Set.to_list in

    let e' = expr extra_vars rels' t1 t2 e in
    ((lbl1, s1), (lbl2, s2), e') in

  G.map_conns conn graph

let writes g =
  let rec find_writes e = match e with
    | E.Bi_op (E.Eq, (E.Var (v, s)), y)
      when String_ext.contains v "wsrc" -> [(v, s)]
    | E.Many_op (E.And, xs) -> List.map find_writes xs |> List.concat
    | _ -> []
  in
  G.map_edges find_writes g

let field_eq = (=)

let label_of_node = fst
let sort_of_node  = snd

let nil_location = -2
let locations st =
  G.nodes st.graph
  |> List.sort_uniq (fun n1 n2 -> compare (label_of_node n1) (label_of_node n2))
  |> List.map label_of_node 

let location_name = string_of_int
let location_eq = (=)

let predecessors g n = List.map label_of_node (G.parents g n)

let nil_result_sort,nilresults = mk_enum "NilResult" ["nilresult"]
let nilresult = List.hd nilresults

let result_sort g n =
  if n = nil_location then nil_result_sort
  else
    g
    |> G.nodes
    |> List.filter (fun node -> label_of_node node =  n)
    |> List.hd
    |> sort_of_node

let transition g lbl1 lbl2 =
  g |> G.conns
  |> List.filter (fun ((lbl1', _, _, _), (lbl2', _, _, _), e) ->
      lbl1 = lbl1' && lbl2 = lbl2')
  |> List.map (fun (_, _, e) -> e)

let nwrites st lbl =
  st.writes |> List.filter (fun (l, _, _, _) -> lbl = l) |> List.length

let ts = ("TS", E.Int)

let remove_structure g =
  G.map_edges (function
      | PG.Assert (e, at) -> e
      | PG.Body e -> e) g

let store_timestamps writes (this, next, i) =
  match i with
  | Ir.ArrAssign (arr, idx, e) ->
    let field = fst arr in
    let src = ("wsrc", E.sort_of e) in
    let time = ("wtime" ^ string_of_int this, E.Int) in

    writes := (this, field, src, time) :: !writes;

    let body = [ time, E.Var ts
               ; LS.update_arr arr idx (E.Var ts)
               ; ts, E.mk_add (E.Var ts) (E.Int_lit 1)
               ; src, e
               ] in

    Some (G.singleton (this, next, PG.Body (E.True, body)))

  | _ -> None

let greatest_under n xs =
  let maximum xs = List.fold_left max (List.nth xs 0) xs in
  List.filter (fun x -> x < n) xs |> maximum

let derive classpath class_name =
  (** The writes are originally placed at the location where they occur. However,
      the graph is rearragned. Therefore, we need to rebind the writes to the
      closest relevant location. *)
  let assoc_writes st =
    let locs = locations st in
    let rebind_location (loc, field, src, time) =
      (greatest_under loc locs, field, src, time) in
    { st with writes = List.map rebind_location st.writes } in

  let writes = ref [] in

  let graph
    : (PG.lbl * E.var list, (E.t, E.t) PG.path) G.t
    = Inspect.inspect (store_timestamps writes) classpath class_name in

  let graph' = graph |> remove_structure in
  let graph'' = graph'
                |> mark_datatypes
                |> translate in
  assoc_writes { graph = graph''
               ; writes = !writes
               }
