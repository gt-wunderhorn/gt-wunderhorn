type t = Int | Bool | Real | Array of t

let inner = function
  | Array t -> t
  | t       -> t

let rec type_to_str = function
  | Int -> "Int"
  | Bool -> "Bool"
  | Real -> "Real"
  | Array (kind) -> Printf.sprintf "[%s]" (type_to_str kind)
