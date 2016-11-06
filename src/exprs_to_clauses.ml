module PG = Program_graph
module G = Graph
module E = Expr

let translate g =
  let translate' ((lbl1, vs1), (lbl2, vs2), edge) =

    let vs1' = List.map (fun v -> E.Var v) vs1 in
    let vs2' = List.map (fun v -> E.Var v) vs2 in

    let precondition = E.Relation (lbl1, vs1') in
    let edge' = match edge with
    | PG.Assert (e, at) ->
      E.mk_impl
        (E.mk_not (E.mk_impl precondition e))
        (E.Query (lbl1, e, at))
    | PG.Body e ->
      E.mk_impl
        (E.mk_and [precondition; e])
        (E.Relation (lbl2, vs2')) in
    (lbl1, lbl2, edge')
  in
  G.map_conns translate' g
