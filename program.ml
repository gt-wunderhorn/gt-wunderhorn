type label = int

type variable = Variable of string

type expression =
  | Relation of label * variable list
  | Query    of variable
  | Int_lit  of int
  | Var      of variable
  | Eq       of expression * expression
  | Implies  of expression * expression
  | And      of expression list
  | Not      of expression
  | Invoke   of expression list * routine

and linear_instruction =
  | Assign  of variable * expression
  | Call    of expression
  | Return  of expression
  | Assert  of variable

and non_linear_instruction =
  | If      of expression * label
  | Goto    of label

and instruction =
  | Linear     of linear_instruction
  | Non_linear of non_linear_instruction

and routine =
  | Routine of variable list * instruction list * variable

module Var_set = Set_ext.Make(
  struct type t = variable;; let compare = compare;; end)

module Rel_set = Set_ext.Make(
  struct
    type t = label * variable list
    let compare (lbl1, _) (lbl2, _) = compare lbl1 lbl2
  end)

let rec expr_vars = function
  | Relation (_, vs) -> Var_set.unions_map Var_set.singleton vs
  | Query v          -> Var_set.singleton v
  | Int_lit _        -> Var_set.empty
  | Var v            -> Var_set.singleton v
  | Eq (e1, e2)      -> Var_set.union (expr_vars e1) (expr_vars e2)
  | Implies (e1, e2) -> Var_set.union (expr_vars e1) (expr_vars e2)
  | And es           -> Var_set.unions_map expr_vars es
  | Not e            -> expr_vars e
  | Invoke (es, _)   -> Var_set.unions_map expr_vars es

let rec expr_rels = function
  | Relation (l, vs) -> Rel_set.singleton (l, vs)
  | Query v          -> Rel_set.empty
  | Int_lit _        -> Rel_set.empty
  | Var v            -> Rel_set.empty
  | Eq (e1, e2)      -> Rel_set.union (expr_rels e1) (expr_rels e2)
  | Implies (e1, e2) -> Rel_set.union (expr_rels e1) (expr_rels e2)
  | And es           -> Rel_set.unions_map expr_rels es
  | Not e            -> expr_rels e
  | Invoke (es, _)   -> Rel_set.unions_map expr_rels es

let instruction_variables = function
  | Assign (v, e) -> Var_set.add v (expr_vars e)
  | Call e        -> expr_vars e
  | Return e      -> expr_vars e
  | Assert v      -> Var_set.singleton v

let instruction_mutables = function
  | Assign (v, _) -> Var_set.singleton v
  | _             -> Var_set.empty

let mk_not = function
  | Not e -> e
  | e     -> Not e

let mk_and = function
  | [e] -> e
  | es  -> And es
