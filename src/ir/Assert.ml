type t =
  | Div0
  | Null
  | NegArray
  | ArrayBound
  | User

let error_code = function
  | User        -> 64
  | Div0        -> 65
  | Null        -> 66
  | NegArray    -> 67
  | ArrayBound  -> 68
