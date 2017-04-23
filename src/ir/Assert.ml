type t =
  | Div0
  | Null
  | NegArray
  | ArrayBound
  | Equivalence
  | User

let error_code = function
  | User        -> 64
  | Div0        -> 65
  | Null        -> 66
  | NegArray    -> 68
  | ArrayBound  -> 69
  | Equivalence -> 70
