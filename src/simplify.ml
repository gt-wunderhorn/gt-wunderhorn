module E = Expr
module G = Graph
module PG = Program_graph

let remove_simple_equalities g =
  let remove clause =
    let is_simp = function
      | E.Var _ | E.Int_lit _ | E.Real_lit _ | E.True | E.False -> true
      | _ -> false
    in

    let is_sa = function
      | E.Bi_op (E.Eq, e1, e2) -> is_simp e1 && is_simp e2
      | _ -> false
    in

    let find_sa es =
      let sas = List.filter is_sa es in
      match sas with
      | (E.Bi_op (E.Eq, E.Var v, e)) :: _
      | (E.Bi_op (E.Eq, e, E.Var v)) :: _ -> Some (v, e)
      | _ -> None
    in

    let rec replace_sa v e e' =
      let rec re = function
        | E.Relation (l, es)      -> E.Relation (l, List.map re es)
        | E.Query (l, e', at)     -> E.Query (l, re e', at)
        | E.Var v'                -> if v = v' then e else E.Var v'
        | E.Un_op (op, e')        -> E.Un_op (op, re e')
        | E.Bi_op (op, e1, e2)    -> E.Bi_op (op, re e1, re e2)
        | E.Many_op (op, es)      -> E.Many_op (op, List.map re es)
        | E.ArrStore (e1, e2, e3) -> E.ArrStore (re e1, re e2, re e3)
        | E.ArrSelect (e1, e2)    -> E.ArrSelect (re e1, re e2)
        | e                       -> e
      in
      match e' with
      | E.Bi_op (E.Eq, e1, e2) ->
        if ((E.Var v) = e1 && e = e2)
        || ((E.Var v) = e2 && e = e1)
        then []
        else [re e']
      | _ -> [re e']
    in

    let loop clause =
      match clause with
      | (E.Bi_op (E.Impl, E.Many_op (E.And, es), r)) ->
        let (es', r') =
          match find_sa es with
          | None -> (es, r)
          | Some (v, e) ->
            ( List.map (replace_sa v e) es |> List.concat
            , replace_sa v e r |> List.hd)
        in
        E.mk_impl (E.mk_and es') r'
      | c -> c
    in
    Algorithm.converge (=) loop clause
  in
  G.map_edges remove g

(** If there is a node in the graph which does not lead to an assertion, it can
    be removed safely *)
let remove_non_asserting_nodes g =
  let edge_is_assertion = function
    | PG.Assert _ -> true
    | _ -> false in

  let has_assertion =
    List.exists (fun (i, t, e) -> edge_is_assertion e) in

  let leads_to_assertion n =
    List.exists has_assertion (G.walks_from g n) in

  G.filter_conns (fun (i, t, e) ->
      edge_is_assertion e || leads_to_assertion t) g

let concatenate_consecutive_paths g =
  let concat (e1, n, e2) = match (e1, e2) with
    | (PG.Body (p, as1), PG.Body (E.True, as2)) ->
      Some (PG.Body (p, as1 @ as2))
    | _ -> None
  in
  G.splice concat g

let remove_empty_paths g =
  let remove (i, t, e) =  match e with
    | PG.Body (_, []) -> Some t
    | _ -> None
  in
  G.pinch remove g

let remove_empty_exprs g =
  let remove (i, t, e) =  match e with
    | PG.Body E.True -> Some t
    | _ -> None
  in
  G.pinch remove g
