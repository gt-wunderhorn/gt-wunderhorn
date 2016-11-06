module L = Lang
module PG = Program_graph

type class_type = L.expr
type field_name = string

type comp = L.expr -> L.expr -> L.expr

type proc =
  { id       : string
  ; entrance : PG.lbl
  ; exit     : PG.lbl
  ; params   : L.var list
  ; return   : L.var
  ; content  : instr list
  }
and ir =
  | Assign      of L.var * L.expr
  | ArrAssign   of L.var * L.expr * L.expr
  | Goto        of PG.lbl
  | If          of comp * L.expr * L.expr * PG.lbl
  | Return      of PG.lbl * L.var * L.expr
  | New         of proc * L.var * class_type * L.expr list
  | NewArray    of L.var * class_type * L.expr list
  | Invoke      of proc * L.var * L.expr list
  | Dispatch    of L.expr * (class_type * proc) list * L.var * L.expr list
  | Assert      of L.expr * PG.assert_type
and instr = PG.lbl * PG.lbl * ir
