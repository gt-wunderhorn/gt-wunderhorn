module G = Graph
module PG = ProgramGraph
module E = Expr
module QID = QualifiedIdentity
module Set = Core.Std.Set.Poly

module AliasTable = CountTable.Make(
  struct
    type t = Var.t
    let compare = compare
  end)

let lookup table v = Var.specify v (string_of_int (AliasTable.get table v))

let rec substitute table e =
  let subst e = substitute table e in
  let special = function
    | E.Var v           -> Some (E.Var (lookup table v))
    | E.Store (v, i, e) -> Some (E.Store (subst v, subst i, subst e))
    | E.Select (v, i)   -> Some (E.Select (subst v, subst i))
    | _       -> None
  in
  E.map special e

let alias_conn ((lbl1, vs1), (lbl2, vs2), edge) =
  let table = AliasTable.empty () in
  let substitute = substitute table in

  let vs1' = List.map (lookup table) vs1 in

  let assign_to_expr (v, e) =
    (* Calculating the rhs needs to occur before incrementing the variable *)
    let rhs = substitute e in
    AliasTable.increment table v;
    E.mk_eq (E.Var (lookup table v)) rhs in

  let convert_assignments e vs =
    let e' = substitute e in
    E.mk_and (e' :: List.map assign_to_expr vs) in

  let sub_vars vs = List.map (fun v -> substitute (E.Var v)) vs in

  let edge' = match edge with
    | PG.Assert (e, at) -> Some (PG.EAssert (substitute e, at))
    | PG.Body (e, vs) -> Some (PG.EBody (convert_assignments e vs))
    | PG.CallLink (exit, amb_in, amb_out, args, ret) ->
      let args' = List.map (fun (v, e) -> substitute e) args in

      let ambient = sub_vars amb_in in
      List.iter (AliasTable.increment table) amb_out;
      let ambient' = sub_vars amb_out in
      let ret' = E.Var (lookup table ret) in

      let vs = args' @ ambient @ ambient' @ [ret'] in
      let transfer_rel = E.Relation ((exit, List.map E.type_of vs), vs) in
      Some (PG.EBody transfer_rel)

    | PG.Return (amb_in, amb_out, params, e, vs, ret) ->
      let params'  = params |> List.sort compare |> sub_vars in
      let ambient  = sub_vars amb_in in
      let lhs      = convert_assignments e vs in
      let ambient' = sub_vars amb_out in
      let ret'     = substitute ret in

      let es = params' @ ambient @ ambient' @ [ret'] in
      let transfer_rel = E.Relation ((lbl2, List.map E.type_of es), es) in
      Some (PG.EReturn (lhs, transfer_rel))

    (* The graph edges into and out of procedures are not needed at the expression level. *)
    | _ -> None
  in

  let vs2' = List.map (lookup table) vs2 in
  match edge' with
  | Some e -> [((lbl1, vs1'), (lbl2, vs2'), e)]
  | None   -> []

let translate g =
  List.map alias_conn (G.conns g)
  |> List.concat
  |> G.of_conns
