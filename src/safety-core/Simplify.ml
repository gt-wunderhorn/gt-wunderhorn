module G = Graph
module PG = ProgramGraph
module Set = Core.Std.Set.Poly
module Map = Core.Std.Map.Poly
module Fn = Core.Std.Fn
module QID = QualifiedIdentity

module V = Var
module T = Type
module E = Expr
module I = Instr
module L = Core.Std.List

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


let always_inline ~inlined:_ ~original:_ = true


(* If a relation only appears on the right hand side of a Horn Clause once, then
   other references to that relation can be inlined. *)
let inline_relations heuristic es =
  (* TODO: Check for recursive cycles *)
  let to_clause expr = match E.to_horn_clause expr with
    | Some((b, E.Relation (r, args))) -> Some(((b, args), r))
    | _ -> None
  in
  let is_recursive_relation rel = function
    | E.Relation (r, _) when r = rel -> true
    | _ -> false
  in
  let clauses = L.filter_map es (to_clause) in
  let find_queried = E.fold_union (function
      | E.Query (lbl, _, _) -> Some (Set.singleton lbl)
      | _ -> None)
  in
  let queries = L.fold (L.map es (find_queried)) ~init:Set.empty ~f:Set.union in

  let aggregate_uniques acc ((b, args), head) = match Map.find acc head with
    | None -> if (Set.mem queries @@ Lbl.qualify "q" @@ fst head)
      || (E.contains (is_recursive_relation head) b)
      then Map.add ~key:head ~data:None acc
      else Map.add ~key:head ~data:(Some (b, args)) acc
    | Some (None) -> acc
    | Some (Some _) -> Map.add ~key:head ~data:None acc
  in
  let head_count = List.fold_left (aggregate_uniques) Map.empty clauses in
  let unique_heads = Map.filter_map head_count (Fn.id) in

  let alias ~key:r ~data:(body, args)=
    let deduplicate_args arg (body, args, count) =
      match (List.mem arg args, arg) with
      | (true, E.Var v) ->
        let arg' = E.Var (Var.specify v (string_of_int count)) in
        let equiv = E.mk_and [body; E.mk_eq arg' arg] in
        (equiv, arg' :: args, count + 1)
      (* Usually should not happen, difficult case to handle *)
      | (true, e) when not (E.is_simp e) -> assert false
      | (true, _) | (false, _) -> (body, arg :: args, count)
    in
    let (dedup_body, dedup_args, _) = L.fold_right args ~f:deduplicate_args ~init:(body, [], 0) in
    let special = function
      | E.Var v when List.mem (E.Var v) args -> Some (E.Var (Var.qualify "v" v))
      | _ -> None
    in
    (E.map special dedup_body, List.map (E.map special) dedup_args)
  in
  let aliased_heads = Map.mapi unique_heads alias in

  let replace_body body target replacement =
    let special = function
      | v when v = target -> Some replacement
      | _ -> None
    in
    E.map special body
  in

  let rec replace_relations id_table = function
    | E.Relation (r, args) -> (match Map.find aliased_heads r with
        | Some (b, args') ->
          let arg_vars = List.fold_left (fun set e -> Set.union set @@ E.vars e) Set.empty args' in
          let other_vars = Set.diff (E.vars b) arg_vars in
          let assign_ids map var = match Map.find !id_table (V.strip_prime var) with
            | Some id ->
              id_table := Map.add !id_table ~key:(V.strip_prime var) ~data:(id + 1);
              Map.add map ~key:var ~data:(string_of_int (id + 1))
            | None ->
              id_table := Map.add !id_table ~key:(V.strip_prime var) ~data:0;
              Map.add map ~key:var ~data:"0"
          in
          let table = Set.fold other_vars ~init:Map.empty ~f:assign_ids in
          let alias_other_vars = function
            | E.Var v -> (match Map.find table v with
                | Some id -> Some (E.Var (Var.primed v id))
                | None -> None)
            | _ -> None
          in
          let body = E.map alias_other_vars b in
          let body' = List.fold_left2 replace_body body args' args in
          let inlined = E.map (replace_relations id_table) body' in
          (* TODO: add heuristic *)
          (* watch out, don't remove the relation if the heuristic says not to inline *)
          Some inlined
        | None -> None)
    | _ -> None
  in

  let map_body expr = match E.to_horn_clause expr with
    | None -> Some expr
    | Some (_, E.Relation (r, _)) when Map.mem unique_heads r -> None
    | Some (body, head) ->
      let id_table = ref Map.empty in
      let collect_ids = function
        | E.Var var ->
          let stripped_var = V.strip_prime var in
          let var_prime = V.default_prime_count var in
          (match Map.find !id_table stripped_var with
           | Some id ->
             id_table := Map.add !id_table ~key:stripped_var ~data:(max id var_prime);
             None
           | None ->
             id_table := Map.add !id_table ~key:stripped_var ~data:var_prime;
             None
          )
        | _ -> None
      in
      let _ = (E.map (collect_ids) body) in
      Some (E.from_horn_clause (E.map (replace_relations id_table) body) head)
  in

  L.filter_map es map_body

