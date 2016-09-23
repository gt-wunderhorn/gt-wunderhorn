type line = Line of int
type variable = Variable of string

type expr =
  | Int_lit of int
  | Var     of variable
  | Eq      of expr * expr
  | Not     of expr

let rec expr_variables = function
  | Int_lit _   -> []
  | Var v       -> [v]
  | Eq (e1, e2) -> expr_variables e1 @ expr_variables e2
  | Not e       -> expr_variables e

type instr =
  | Assign of variable * expr
  | If     of expr * line
  | Goto   of line
  | Assert of variable

let instr_variables = function
  | Assign (v, e) -> v :: expr_variables e
  | If (e, _)     -> expr_variables e
  | Goto _        -> []
  | Assert v      -> [v]
