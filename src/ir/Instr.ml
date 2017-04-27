module E = Expr

type 'a proc =
  { id       : QualifiedIdentity.t
  ; params   : Var.t list
  ; ret_type : Type.t
  ; content  : ('a list) Lazy.t
  }

and ir =
  | Assign of Var.t * E.t
  | Goto   of Lbl.t
  | If     of E.t * Lbl.t
  | Return of E.t
  | Invoke of t proc * Var.t * E.t list
  | Assert of E.t * E.query

and t = Instr of Lbl.t * ir

let map_content f p = { p with content = Lazy.from_fun (fun _ -> f (Lazy.force p.content)) }

let entrance proc = Lbl.At (proc.id, Lbl.Entrance)
let exit     proc = Lbl.At (proc.id, Lbl.Exit)

let mk_assign v e = (v, e)
