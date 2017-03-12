type t = Int | Bool | Real | Array of t

let inner = function
  | Array t -> t
  | t       -> t

let is_scalar = function
  | Array _ -> false
  | _ -> true

let rec type_to_str = function
  | Int -> "Int"
  | Bool -> "Bool"
  | Real -> "Real"
  | Array (kind) -> Printf.sprintf "[%s]" (type_to_str kind)
