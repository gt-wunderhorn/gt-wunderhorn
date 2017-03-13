module J = Sawja_pack.JBir
module JB = Javalib_pack.JBasics
module QID = QualifiedIdentity

type t =
  { name     : QID.t
  ; params   : (JB.value_type * J.var) list
  ; content  : J.instr list
  ; ret_type : JB.value_type option
  ; sign     : JB.method_signature
  ; vartable : (int * int * string * JB.value_type * int) list option
  ; cl_name  : JB.class_name
  }
