module G = Graph
module Set = Core.Std.Set.Poly

type lbl = int

type assert_type =
  | Div0
  | Null
  | NegArray
  | ArrayBound
  | User
let show_assert_type = function
  | Div0       -> "Division by 0?"
  | Null       -> "Null?"
  | NegArray   -> "Array with negative size?"
  | ArrayBound -> "Array out of bound?"
  | User       -> "User specified:"

type ('a, 'b) path =
  | Assert of 'a * assert_type
  | Body of 'b

let map_body f g =
  let f_edge = function
    | Assert (a, t) -> Assert (a, t)
    | Body b -> Body (f b)
  in
  G.map_edges f_edge g
