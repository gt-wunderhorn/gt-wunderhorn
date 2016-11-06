module PG = Program_graph
module G = Graph
module L = Lang

let translate g : (L.lbl, L.expr) G.t =
  let translate' ((lbl1, vs1), (lbl2, vs2), edge) =

    let vs1' = List.map (fun v -> L.Var v) vs1 in
    let vs2' = List.map (fun v -> L.Var v) vs2 in

    let precondition = L.Relation (lbl1, vs1') in
    let edge' = match edge with
    | PG.Assert (e, at) ->
      L.mk_impl
        (L.mk_not (L.mk_impl precondition e))
        (L.Query (lbl1, e, at))
    | PG.Body e ->
      L.mk_impl
        (L.mk_and [precondition; e])
        (L.Relation (lbl2, vs2')) in
    (lbl1, lbl2, edge')
  in
  G.map_conns translate' g
