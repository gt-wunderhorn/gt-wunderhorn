module G = Graph
module PG = ProgramGraph
module Set = Core.Std.Set.Poly
module Map = Core.Std.Map.Poly
module QID = QualifiedIdentity

module V = Var
module T = Type
module E = Expr
module I = Instr

(* If a relation never appears on the rhs of a horn clause, then it can be safely
   removed from any expression which refers to it. *)
let remove_unaffecting_relations es =
  let rhs_rels = ref Set.empty in

  let get_rhs_rels e = match E.to_horn_clause e with
    | Some (lhs, E.Relation (r, args)) ->
      rhs_rels := Set.union !rhs_rels (E.rels (E.Relation (r, args)))
    | _ -> ()
  in

  let replace e =
    let special = function
      | E.Relation (r, es) ->
        Some (if Set.mem !rhs_rels r
              then E.Relation (r, es)
              else E.Bool true)
      | _ -> None
    in
    E.map special e
  in

  List.iter get_rhs_rels es;
  List.map replace es

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
  let concat ((ps1, e1), n, (ps2, e2)) = match (e1, e2) with
    | (PG.Body (p, as1), PG.Body (E.Bool true, as2)) ->
      Some (ps1, PG.Body (p, as1 @ as2))
    | ( PG.Body (p, as1)
      , PG.Return (ent, ex, ps, E.Bool true, as2, value)) ->
      Some (ps1, PG.Return (ent, ex, ps, p, as1 @ as2, value))
    | _ -> None
  in
  G.splice concat g

let concatenate_consecutive_exprs g =
  let concat (e1, n, e2) = match (e1, e2) with
    | (PG.EBody e1, PG.EBody e2) ->
      Some (PG.EBody (E.mk_and [e1;e2]))
    | (PG.EBody e1, PG.EReturn (e2, rhs)) ->
      Some (PG.EReturn (E.mk_and [e1;e2], rhs))
    | _ -> None
  in
  G.splice concat g

let remove_empty_paths g =
  let remove (i, t, e) = match e with
    | (_, PG.Body (_, [])) -> Some t
    | _ -> None
  in
  G.pinch remove g

let remove_empty_exprs g =
  let remove (i, t, e) =  match e with
    | PG.EBody E.Bool true -> Some t
    | _ -> None
  in
  G.pinch remove g

(* If a relation only appears on the right hand side of a Horn Clause once, then
   other references to that relation can be inlined. *)
let inline_relations es =
  let loop es =
    let appearances (once, more) e = match E.to_horn_clause e with
      | Some (_, E.Relation (r, _)) ->
        if List.mem r more
        then (once, more)
        else if List.mem r once
        then (List.filter ((<>) r) once, r :: more)
        else (r :: once, more)
      | _ -> (once, more) in
    let appear_once = fst (List.fold_left appearances ([], []) es) in

    let collect_bodies m e = match E.to_horn_clause e with
      | Some (lhs, E.Relation (r, args)) ->
        if List.mem r appear_once
        then Map.add m r (lhs, args)
        else m
      | _ -> m in
    let bodies = ref (List.fold_left collect_bodies Map.empty es) in

    let substitute es =
      let alias =
        let special = function
          | E.Var v -> Some (E.Var (Var.qualify "v" v))
          | _ -> None in
        E.map special in

      bodies := Map.map !bodies (fun (lhs, args) -> (alias lhs, List.map alias args));

      let subst e =
        let special = function
          | E.Relation (r, args) ->
            Some (match Map.find !bodies r with
                | Some (lhs, args') ->
                  E.mk_and (lhs :: List.map2 E.mk_eq args args')
                | _ -> E.Relation (r, args))
          | _ -> None
        in

        let special e = match E.to_horn_clause e with
          | Some (lhs, rhs) ->
            Some (E.from_horn_clause (E.map special lhs) rhs)
          | _ -> None in
        E.map special e
      in
      List.map subst es
    in

    let is_removable e = match E.to_horn_clause e with
      | Some (lhs, E.Relation (r, args)) -> List.mem r appear_once
      | _ -> false in

    let targets = List.filter (fun e -> not (is_removable e)) es in

    Algorithm.converge (=) substitute targets
  in
  (* loop es *)
  Algorithm.converge (=) loop es
