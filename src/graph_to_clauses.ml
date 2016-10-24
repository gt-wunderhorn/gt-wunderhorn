module L = Lang

module Alias_table = Count_table.Make(
  struct
    type t = L.var
    let compare = compare
  end)

let mk_alias table (name, sort) =
  (name ^ "_" ^ (string_of_int (Alias_table.get table (name, sort))), sort)

let substitute table =
  let rec su = function
    | L.Var v              -> L.Var (mk_alias table v)
    | L.Un_op (o, e)       -> L.Un_op (o, su e)
    | L.Bi_op (o, e1, e2)  -> L.Bi_op (o, su e1, su e2)
    | L.Many_op (o, es)    -> L.Many_op (o, List.map su es)
    | L.Query (loc, e)     -> L.Query (loc, su e)
    | L.Relation (lbl, es) -> L.Relation (lbl, List.map su es)
    | L.ArrSelect (a, i)   -> L.ArrSelect (su a, su i)
    | L.ArrStore (a, i, e) -> L.ArrStore (su a, su i, su e)
    | e                    -> e
  in su

let mk_condition table label vars =
  if L.V_set.is_empty vars
  then L.True
  else
    L.Relation (label, List.map
                  (fun v -> L.Var (mk_alias table v))
                  (L.V_set.elements vars))

let reduce_instr p table = function
  | L.Relate lbl    -> mk_condition table lbl (L.critical_vars p lbl)
  | L.Call e        -> substitute table e
  | L.Assert _      -> assert false
  | L.Assign (v, e) ->
    (* Calculating the rhs needs to occur before incrementing the variable *)
    let rhs = substitute table e in
    Alias_table.increment table v;
    L.Bi_op (L.Eq, L.Var (mk_alias table v), rhs)

let translate_path p (init, term, path) =
  let initial_vars  = L.critical_vars p init in
  let terminal_vars = L.critical_vars p term in
  let table = Alias_table.empty () in

  let precondition = mk_condition table init initial_vars in

  match path with
  | [L.Assert e] ->
    L.mk_impl
      (L.mk_not (L.mk_impl precondition (substitute table e)))
      (substitute table (L.Query (init, e)))

  | path ->
    let expressions = List.map (reduce_instr p table) path in

    let lhs =
      if L.V_set.is_empty initial_vars
      then L.mk_and expressions
      else L.mk_and (precondition :: expressions) in

    let postcondition = mk_condition table term terminal_vars in
    L.Bi_op (L.Impl, lhs, postcondition)

let translate p =
  let edges = L.PG.elements p in
  List.map (translate_path p) edges
