module PG = ProgramGraph
module G = Graph
module E = Expr
module QID = QualifiedIdentity

let translate g =
  let translate' ((lbl1, vs1), (lbl2, vs2), edge) =
    let vs1' = List.map (fun v -> E.Var v) vs1 in
    let vs2' = List.map (fun v -> E.Var v) vs2 in

    let ts1 = List.map E.type_of vs1' in
    let ts2 = List.map E.type_of vs2' in

    let pre = E.Relation ((lbl1, ts1), vs1') in

    let post = match lbl2 with
      | Lbl.At (_, Lbl.Exit) -> (E.Relation ((lbl2, ts1 @ ts2), vs1' @ vs2'))
      | _ -> (E.Relation ((lbl2, ts2), vs2')) in

    let horn_clause e = E.mk_impl (E.mk_and [pre; e]) post in

    match edge with
      | PG.EAssert (e, q) ->
        E.mk_impl
          (E.mk_not (E.mk_impl pre e))
          (E.Query (Lbl.qualify "q" lbl1, q, e))
      | PG.EBody e -> horn_clause e
      | PG.EReturn (lhs, rhs) -> E.mk_impl (E.mk_and [pre; lhs]) rhs
  in
  List.map translate' (G.conns g)
