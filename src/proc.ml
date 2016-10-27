module L = Lang
module J = Sawja_pack.JBir
module JB = Javalib_pack.JBasics

type t =
  { name     : string
  ; id       : string
  ; params   : (JB.value_type * J.var) list
  ; content  : J.instr list
  ; ret_type : JB.value_type option
  ; sign     : JB.method_signature
  ; cl_name  : JB.class_name
  }
