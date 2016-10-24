module L = Lang

type class_type = L.expr
type field_name = string

type comp = L.expr -> L.expr -> L.expr

type proc =
  { entrance : L.lbl
  ; exit     : L.lbl
  ; params   : L.var list
  ; return   : L.var
  ; content  : instr list
  }
and ir =
  | Assign      of L.var * L.expr
  | ArrAssign   of L.var * L.expr * L.expr
  | Goto        of L.lbl
  | If          of comp * L.expr * L.expr * L.lbl
  | Return      of L.lbl * L.var * L.expr
  | New         of proc * L.var * class_type * L.expr list
  | NewArray    of L.var * class_type * L.expr list
  | Invoke      of proc * L.var * L.expr list
  | Dispatch    of L.expr * (class_type * proc) list * L.var * L.expr list
  | Assert      of L.expr
and instr = L.lbl * L.lbl * ir

let ir_exprs = function
  | Assign (_, e)          -> [e]
  | ArrAssign (_, i, e)    -> [i; e]
  | Goto _                 -> []
  | If (_, e1, e2, _)      -> [e1; e2]
  | Return (_, _, e)       -> [e]
  | New (_, _, _, es)      -> es
  | NewArray (_, _, es)    -> es
  | Invoke (_, _, es)      -> es
  | Dispatch (o, _, _, es) -> o :: es
  | Assert _               -> []
