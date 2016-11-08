module PG = Program_graph
module G = Graph
module E = Expr
module LS = Lang_state

module Set = Core.Std.Set.Poly
module CZ = Clauses_to_z3
module SS = Shape_state

open Z3_type

let ctx = CZ.ctx

let modify_field_use e =
  let ts_count = ref (-1) in
  let ts _ = E.Var ("TS" ^ string_of_int !ts_count, E.Int) in
  let nts _ = E.Var ("TS" ^ string_of_int (!ts_count + 1), E.Int) in

  let current = ref (-1) in

  let reads = ref [] in
  let writes = ref [] in
  let extra_eqs = ref [] in
  let special = function
    (** Record any array selects. They should also be uniquely named. The selection
        itself should be replaced with a new read destination. *)
    | E.FieldSelect ((name, s), e) ->
      let suffix = string_of_int !current in
      let field = name in
      let dst = ("rdst" ^ suffix, s) in
      let time = ("rtime" ^ suffix, E.Int) in

      current := !current + 1;
      reads := (field, dst, time) :: !reads;

      extra_eqs := E.mk_eq (E.Var time) (E.FieldSelect ((name, s), e)) :: !extra_eqs;

      Some (E.Var dst)

    | E.FieldStore ((name, s), i, e) ->
      let suffix = string_of_int !current in
      let field = name in
      let src = ("wsrc" ^ suffix, E.sort_of e) in
      let time = ("wtime" ^ suffix, E.Int) in

      current := !current + 1;
      ts_count := !ts_count + 1;
      writes := (field, src, time) :: !writes;

      extra_eqs :=
        [ E.mk_eq (E.Var time) (ts ())
        ; E.mk_eq (nts ()) (E.mk_add (ts ()) (E.Int_lit 1))
        ; E.mk_eq (E.Var src) e
        ] @ !extra_eqs;
      Some (E.FieldStore ((name, s), i, (ts ())))

    | _ -> None
  in

  let e' = E.map special e in
  (!reads, !writes, E.mk_and (e' :: !extra_eqs))

(* let translate graph = *)
(*   let union_map_list f xs = Set.union_list (List.map f xs) in *)
(*   let r_union_map_list f xs = List.fold_left E.R_set.union E.R_set.empty (List.map f xs) in *)

(*   let rec expr extra_vars rels t1 t2 e = *)
(*     fun (v1 : Z3.Expr.expr) (v2 : Z3.Expr.expr) -> *)
(*       let assignments = List.map *)
(*           (fun v -> CZ.bi_op E.Eq ctx (assign v t1 v1) (assign v t2 v2)) extra_vars in *)

(*       let special = function *)
(*         | E.Var (name, _) -> Some (if has_acsor name t1 *)
(*                                    then Z3.Expr.mk_app ctx (get_acsor name t1) [v1] *)
(*                                    else Z3.Expr.mk_app ctx (get_acsor name t2) [v2]) *)
(*         | _ -> None in *)

(*       let e' = CZ.expr special rels [] e in *)
(*       CZ.many_op E.And ctx (assignments @ [e']) *)
(*   in *)

(*   let graph = G.map_edges modify_field_use graph in *)
(*   let exprs = G.edges graph |> List.map (fun (_, _, x) -> x) in *)

(*   let rels = r_union_map_list E.rels exprs |> E.R_set.elements in *)
(*   let queries = union_map_list E.queries exprs |> Set.to_list in *)

(*   let rels' = *)
(*     List.map (fun (lbl, es) -> CZ.relation ("r_" ^ string_of_int lbl, es)) rels *)
(*     @ *)
(*     List.map (fun (lbl, e, at) -> CZ.relation ("q_" ^ string_of_int lbl, [e])) queries in *)

(*   let conn ((lbl1, vs1, s1, t1), (lbl2, vs2, s2, t2), (reads, e)) = *)
(*     let extra_vars = *)
(*       Set.inter (Set.of_list vs1) (Set.of_list vs2) |> Set.to_list in *)

(*     let e' = expr extra_vars rels' t1 t2 e in *)
(*     ((lbl1, s1, t1), (lbl2, s2, t2), (reads, e')) in *)

(*   G.map_conns conn graph *)

(* let derive classpath class_name = *)
(*   let ts = ("TS", E.Int) in *)

(*   let store_timestamps writes (this, next, i) = *)
(*     match i with *)
(*     | Ir.ArrAssign (arr, idx, e) -> *)
(*       let suffix = string_of_int this in *)
(*       let field = fst arr in *)
(*       let src = ("wsrc" ^ suffix, E.sort_of e) in *)
(*       let time = ("wtime" ^ suffix, E.Int) in *)

(*       writes := (this, field, src, time) :: !writes; *)

(*       let body = [ (time, E.Var ts) *)
(*                  ; (LS.update_arr arr idx (E.Var ts)) *)
(*                  ; (ts, E.mk_add (E.Var ts) (E.Int_lit 1)) *)
(*                  ; (src, e) *)
(*                  ] in *)

(*       Some (G.singleton (this, next, PG.Body (E.True, body))) *)

(*     | _ -> None *)
(*   in *)

(*   let remove_structure g = *)
(*     G.map_edges (function *)
(*         | PG.Assert (e, at) -> e *)
(*         | PG.Body e -> e) g in *)

(*   let assoc_writes st = *)
(*     let greatest_under n xs = *)
(*       let maximum xs = List.fold_left max (List.nth xs 0) xs in *)
(*       List.filter (fun x -> x < n) xs |> maximum in *)

(*     let locs = SS.locations st in *)
(*     let rebind_location (loc, field, src, time) = *)
(*       (greatest_under loc locs, field, src, time) in *)
(*     { st with SS.writes = List.map rebind_location st.SS.writes } in *)

(*   let writes = ref [] in *)

(*   let graph *)
(*     : (PG.lbl * E.var list, (E.t, E.t) PG.path) G.t *)
(*     = Inspect.inspect (store_timestamps writes) classpath class_name in *)

(*   let graph' = graph |> remove_structure in *)
(*   let graph'' = graph' *)
(*                 |> mark_datatypes *)
(*                 |> translate in *)
(*   assoc_writes { graph = graph'' *)
(*                ; SS.writes = !writes *)
(*                } *)
