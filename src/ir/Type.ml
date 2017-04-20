type t = Int | Bool | Real | Array of t

let inner = function
  | Array t -> t
  | t       -> t

let is_scalar = function
  | Array _ -> false
  | _ -> true
