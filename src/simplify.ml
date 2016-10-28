module L = Lang

let remove_simple_assignments clause =
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

(** If there is a node in the graph which does not lead to an assertion, it can
    be removed safely *)
let remove_useless_nodes g =
  let is_assertion = function
    | L.Assert _ -> true
    | _ -> false in

  let edge_has_assertion =
    List.exists is_assertion in

  let has_assertion =
    List.exists (fun (i, t, e) -> edge_has_assertion e) in

  let leads_to_assertion n =
    List.exists has_assertion (L.PG.paths_from g n) in

  L.PG.filter (fun (i, t, e) ->
      edge_has_assertion e || leads_to_assertion t) g
