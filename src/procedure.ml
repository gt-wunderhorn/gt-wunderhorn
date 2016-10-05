type 'a t =
  { id       : string
  ; params   : Ir.variable list
  ; ret_sort : Ir.sort
  ; content  : 'a
  }

let map (f : 'a -> 'b) proc =
  { id       = proc.id
  ; params   = proc.params
  ; ret_sort = proc.ret_sort
  ; content  = f (proc.content)
  }

let ret_var p = Ir.Variable (p.id ^ "_ret", p.ret_sort)
let entry_label p = p.id ^ "_0"
let exit_label  p = p.id ^ "_exit"
