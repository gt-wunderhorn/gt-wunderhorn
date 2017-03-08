type t = Int | Bool | Real | Array of t

let inner = function
  | Array t -> t
  | t       -> t
