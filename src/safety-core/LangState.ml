module QID = QualifiedIdentity

module I = Instr
module E = Expr
module T = Type

let rec show_type = function
  | T.Unit     -> "Unit"
  | T.Bool     -> "Bool"
  | T.Int      -> "Int"
  | T.Real     -> "Real"
  | T.Array t  -> "Array_" ^ show_type t

let global n t = Var.Mk (QID.of_list ["g"; n], t)
let is_global = function
  | (Var.Mk (QID.QID ("g" :: _), _)) -> true
  | _ -> false

let array_array t = global ("ARRAY" ^ show_type t) (T.Array (T.Array t))
let class_array   = global "CLASS_TYPE" (T.Array T.Int)
let array_length  = global "ARRAY_LENGTH" (T.Array T.Int)
let id            = global "ID" T.Int
let dummy         = global "DUMMY" T.Int
