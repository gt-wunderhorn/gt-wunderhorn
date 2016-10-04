module P = Program

type 'a t =
  { id      : string
  ; params  : P.variable list
  ; content : 'a
  }

let map (f : 'a -> 'b) proc =
  { id      = proc.id
  ; params  = proc.params
  ; content = f (proc.content)
  }

let ret_var p = P.Variable (p.id ^ "_ret")
let entry_label p = p.id ^ "_0"
let exit_label  p = p.id ^ "_exit"
