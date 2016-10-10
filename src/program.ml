type var = string

type expr =
  | Var of var

type loc = Loc of int

type assign = var * expr

type proc_info =
  { entrance : loc
  ; exit     : loc
  ; params   : var list
  ; output   : var
  }

type instr =
  | Assign of assign
  | Goto   of loc
  | If     of expr * loc
  | Call   of expr list * var * proc_info

type path = Path of expr * assign list

module Program = Graph.Make(
  struct type node = loc;; type edge = path;; end)
type program = Program.t
