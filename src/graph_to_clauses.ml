open Ir
module T = Trace

module Alias_table = Count_table.Make(
  struct
    type t = variable
    let compare = compare
  end)

let mk_alias table var =
  let Variable (prefix, s) = var in
  Variable (prefix ^ "_" ^ (string_of_int (Alias_table.get table var)), s)

let substitute table =
  let rec subst = function
    | ArrStore (v, i, e) -> ArrStore (subst v, subst i, subst e)
    | ArrSelect (e1, e2) -> ArrSelect (subst e1, subst e2)
    | Relation (lbl, vs) -> Relation (lbl, List.map (mk_alias table) vs)
    | Query e            -> Query (subst e)
    | Var v              -> Var (mk_alias table v)
    | Add (e1, e2)       -> Add (subst e1, subst e2)
    | Eq (e1, e2)        -> Eq (subst e1, subst e2)
    | Ge (e1, e2)        -> Ge (subst e1, subst e2)
    | Gt (e1, e2)        -> Gt (subst e1, subst e2)
    | Le (e1, e2)        -> Le (subst e1, subst e2)
    | Lt (e1, e2)        -> Lt (subst e1, subst e2)
    | Implies (e1, e2)   -> Implies (subst e1, subst e2)
    | And es             -> And (List.map (subst) es)
    | Not e              -> Not (subst e)
    | Int_lit i          -> Int_lit i
    | True               -> True
    | False              -> False in
  subst

let reduce_instr table = function
  | Assign (v, e) ->
    let rhs = substitute table e in
    Alias_table.increment table v;
    let v = mk_alias table v in
    Eq (Var v, rhs)
  | Call e -> substitute table e
  | Assert _ -> assert false

let mk_condition st label vars =
  if Var_set.is_empty vars
  then True
  else Relation (label, List.map (mk_alias st) (Var_set.elements vars))

let translate_path trace (init, term, path) =
  let initial_vars  = T.critical_variables trace init in
  let terminal_vars = T.critical_variables trace term in
  let table = Alias_table.empty () in

  let precondition = mk_condition table init initial_vars in

  match path with
  | T.Assertion v ->
    Implies (Not (Implies (precondition,
                           (Eq (substitute table v, Int_lit 1)))),
             substitute table (Query v))
  | T.Path instrs ->
    let expressions = List.map (reduce_instr table) instrs in

    let lhs =
      if Var_set.is_empty initial_vars
      then mk_and expressions
      else mk_and (precondition :: expressions) in

    let postcondition = mk_condition table term terminal_vars in
    Implies (lhs, postcondition)

let translate trace =
  let edges = T.P_graph.connected_edges trace in
  List.map (translate_path trace) edges
