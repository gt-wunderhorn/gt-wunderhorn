type zexpr = Z3.Expr.expr

type t =
  { label : int
  ; predecessors : int list
  ; result_sort : Z3.Sort.sort
  ; reads  : ((zexpr -> zexpr) Field.field_desc) list
  ; writes : ((zexpr -> zexpr) Field.field_desc) list
  ; transition : zexpr -> zexpr -> zexpr
  }
