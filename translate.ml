open Program
module T = Trace

module Alias_table = Count_table.Make(
  struct
    type t = variable
    let compare = compare
  end)

let mk_alias table var =
  let Variable prefix = var in
  Variable (prefix ^ (string_of_int (Alias_table.get table var)))

let rec substitute table = function
  | Relation (lbl, vs) -> Relation (lbl, List.map (mk_alias table) vs)
  | Query v            -> Query (mk_alias table v)
  | Int_lit i          -> Int_lit i
  | Var v              -> Var (mk_alias table v)
  | Eq (e1, e2)        -> Eq (substitute table e1, substitute table e2)
  | Implies (e1, e2)   -> Implies (substitute table e1, substitute table e2)
  | And es             -> And (List.map (substitute table) es)
  | Not e              -> Not (substitute table e)
  | Invoke _           -> assert false (** TODO *)

let reduce_instr table = function
  | Assign (v, e) ->
    let table = Alias_table.increment table v in
    (substitute table (Eq (Var v, e)), table)
  | Call e   -> (substitute table e, table)
  | Return e -> (substitute table e, table) (** TODO *)
  | Assert (Variable v) ->
    Printf.printf "%s\n" v;
    assert false

let mk_condition table label vars =
  Relation (label, List.map (mk_alias table) (Var_set.elements vars))

let translate_path trace path =
  let initial_vars  = T.critical_variables trace (T.initial_label path) in
  let terminal_vars = T.critical_variables trace (T.terminal_label path) in
  let all_vars = Var_set.unions [initial_vars; terminal_vars; T.path_variables path] in
  let table = Alias_table.mk (Var_set.elements all_vars) in

  let loop (acc, table) instr =
    let (expr, table') = reduce_instr table instr in
    (acc @ [expr], table') in

  let precondition = mk_condition table (T.initial_label path) initial_vars in

  match path with
  | Assertion (_, v) ->
    Implies (Not (Implies (precondition, (Eq (substitute table (Var v), Int_lit 1)))), substitute table (Query v))
  | Path (_, _, instrs) ->
    let (expressions, table) = List.fold_left loop ([], table) instrs in

    let lhs =
      if Var_set.is_empty initial_vars
      then mk_and expressions
      else mk_and (precondition :: expressions) in

    let postcondition = mk_condition table (T.terminal_label path) terminal_vars in
    Implies (lhs, postcondition)

let translate trace = List.map (translate_path trace) trace
