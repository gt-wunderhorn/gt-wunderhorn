module E = Expr
module PG = Program_graph

type class_type = E.t
type field_name = string

type comp = E.t -> E.t -> E.t

type proc =
  { id       : string
  ; entrance : PG.lbl
  ; exit     : PG.lbl
  ; params   : E.var list
  ; return   : E.var
  ; content  : instr list
  }
and ir =
  | Assign      of E.var * E.t
  | ArrAssign   of E.var * E.t * E.t
  | FieldAssign of E.var * E.t * E.t
  | Goto        of PG.lbl
  | If          of comp * E.t * E.t * PG.lbl
  | Return      of PG.lbl * E.var * E.t
  | New         of proc * E.var * class_type * E.t list
  | NewArray    of E.var * class_type * E.t list
  | Invoke      of proc * E.var * E.t list
  | Dispatch    of E.t * (class_type * proc) list * E.var * E.t list
  | Assert      of E.t * PG.assert_type
and instr = PG.lbl * PG.lbl * ir
