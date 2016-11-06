module L = Lang
module G = Graph
module PG = Program_graph

let remove_simple_equalities g =
  let remove clause =
    let is_simp = function
      | L.Var _ | L.Int_lit _ | L.Real_lit _ | L.True | L.False -> true
      | _ -> false
    in

    let is_sa = function
      | L.Bi_op (L.Eq, e1, e2) -> is_simp e1 && is_simp e2
      | _ -> false
    in

    let find_sa es =
      let sas = List.filter is_sa es in
      match sas with
      | (L.Bi_op (L.Eq, L.Var v, e)) :: _
      | (L.Bi_op (L.Eq, e, L.Var v)) :: _ -> Some (v, e)
      | _ -> None
    in

    let rec replace_sa v e e' =
      let rec re = function
        | L.Relation (l, es)      -> L.Relation (l, List.map re es)
        | L.Query (l, e', at)     -> L.Query (l, re e', at)
        | L.Var v'                -> if v = v' then e else L.Var v'
        | L.Un_op (op, e')        -> L.Un_op (op, re e')
        | L.Bi_op (op, e1, e2)    -> L.Bi_op (op, re e1, re e2)
        | L.Many_op (op, es)      -> L.Many_op (op, List.map re es)
        | L.ArrStore (e1, e2, e3) -> L.ArrStore (re e1, re e2, re e3)
        | L.ArrSelect (e1, e2)    -> L.ArrSelect (re e1, re e2)
        | e                       -> e
      in
      match e' with
      | L.Bi_op (L.Eq, e1, e2) ->
        if ((L.Var v) = e1 && e = e2)
        || ((L.Var v) = e2 && e = e1)
        then []
        else [re e']
      | _ -> [re e']
    in

    let loop clause =
      match clause with
      | (L.Bi_op (L.Impl, L.Many_op (L.And, es), r)) ->
        let (es', r') =
          match find_sa es with
          | None -> (es, r)
          | Some (v, e) ->
            ( List.map (replace_sa v e) es |> List.concat
            , replace_sa v e r |> List.hd)
        in
        L.mk_impl (L.mk_and es') r'
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
    | (PG.Body (p, as1), PG.Body (L.True, as2)) ->
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
    | PG.Body L.True -> Some t
    | _ -> None
  in
  G.pinch remove g
