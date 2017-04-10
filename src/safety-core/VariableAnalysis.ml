module G = Graph
module LS = LangState
module PG = ProgramGraph
module Set = Core.Std.Set.Poly
module Map = Core.Std.Map.Poly
module I = Instr
module E = Expr
module QID = QualifiedIdentity

(**
 * A variable is live at a location if:
   * It is assigned to before the location.
   * It is used after the location.
   * The closest usage occurs before a new assignment to the same variable.
   * The variable is either used or assigned at or below the current scope.
 *)

type scope =
  | Caller
  | Callee

let merge m1 m2 =
  let resolve_scope s1 s2 = match (s1, s2) with
    | (Caller, Callee) -> Callee
    | (Callee, Caller) -> Callee
    | (Caller, Caller) -> Caller
    | (Callee, Callee) -> Callee
  in

  let resolve ~key = function
    | `Both (s1, s2) -> Some (resolve_scope s1 s2)
    | `Left s        -> Some s
    | `Right s       -> Some s
  in
  Map.merge m1 m2 ~f:resolve

let remove_locals qid m =
  let is_local (Var.Mk (qid', _)) =
    QID.full_prefix qid' = qid
  in
  let f ~key ~data = not (is_local key) in
  Map.filter m f

let enter qid m = remove_locals qid (Map.map m (fun _ -> Callee))
let exit  qid m = remove_locals qid (Map.map m (fun _ -> Caller))

let build_map f xs = List.fold_left f Map.empty xs

(* Given a path, determine which variables are used before they are assigned to *)
let path_usage p =
  let module St = struct
    type scoped_vars = (Var.t, scope) Map.t
    type t = { assigned : scoped_vars ; used : scoped_vars } end in

  let set_to_callee s = Set.fold s ~init:Map.empty ~f:(fun m v -> Map.add m v Callee) in

  let diff m1 m2 = List.fold_left Map.remove m1 (Map.keys m2) in

  let loop used frag =
    let fragment used e assigns =
      let add_var st (v, e) =
        (* Add the variables in e to the use set if they were not in the assignment set. *)
        let augment e = merge
            (diff (set_to_callee (E.vars e)) st.St.assigned)
            st.St.used in
        { St.assigned = Map.add st.St.assigned v Callee; St.used = augment e }
      in

      let st = { St.assigned = Map.empty ; St.used = used } in
      let st' = List.fold_left add_var st assigns in

      merge (set_to_callee (E.vars e)) st'.St.used
    in

    match frag with
    | PG.Assert (e, _)                -> fragment used e []
    | PG.Body (e, xs)                 -> fragment used e xs
    | PG.ScopeIn (qid, e, xs)         -> fragment (enter qid used) e xs
    | PG.ScopeOut qid                 -> exit qid used
    | PG.CallLink _                   -> used
    | PG.Return (_, _, _, p, xs, rhs) -> fragment used (E.mk_and [p; rhs]) xs
  in
  build_map loop p

let path_assignment p =
  let loop assigned frag =
    let fragment target to_add =
      List.fold_left
        (fun m v -> Map.add m v Callee)
        target
        (List.map fst to_add)
    in

    match frag with
    | PG.Assert _                   -> assigned
    | PG.Body (e, xs)               -> fragment assigned xs
    | PG.ScopeIn (qid, e, xs)       -> fragment (exit qid assigned) xs
    | PG.ScopeOut qid               -> enter qid assigned
    | PG.CallLink (_, _, _, _, v)   -> Map.add assigned v Callee
    | PG.Return (_, _, _, _, xs, _) -> fragment assigned xs
  in
  build_map loop p

let nested_map f xs =
  List.map (List.map f) xs

let live g n =
  let coming = G.walks_to g n |> nested_map G.edge in
  let going = G.walks_from g n |> nested_map G.edge in

  let all_assigns = build_map merge (List.map path_assignment coming) in
  let all_usage   = build_map merge (List.map path_usage going) in

  let is_relevant var =
    match (Map.find all_usage var, Map.find all_assigns var) with
    | (Some Caller, Some Caller) -> false
    | (Some _     , Some _     ) -> true
    | _                          -> false
  in

  Map.keys all_assigns
  |> List.filter is_relevant

let annotate_nodes (g : (Lbl.t, (Var.t list * Lbl.t PG.instr_path)) Graph.t) =
  let conn (n1, n2, e) =
    let (params, e) = e in
    let g = G.map_edges snd g in
    let ambient lbl params =
      let inputs = live g lbl in
      Set.to_list
        (Set.diff
           (Set.of_list inputs)
           (Set.of_list params))
    in
    let e' = match e with
      | PG.CallLink (lbl, entr, exit, args, v) ->
        let params = List.map fst args in
        PG.CallLink (lbl, ambient entr params, ambient exit params, args, v)

      | PG.Return (entr, exit, params, e, b, v) ->
        PG.Return (ambient entr params, ambient exit params, params, e, b, v)
      | PG.Assert (e, at) -> PG.Assert (e, at)
      | PG.Body (p, assigns) -> PG.Body (p, assigns)
      | PG.ScopeIn (qid, p, assigns) -> PG.ScopeIn (qid, p, assigns)
      | PG.ScopeOut qid -> PG.ScopeOut qid
    in

    let vs1 = Algorithm.nub (params @ live g n1) in
    let vs2 = Algorithm.nub (params @ live g n2) in

    ((n1, vs1), (n2, vs2), e')
  in
  G.map_conns conn g
