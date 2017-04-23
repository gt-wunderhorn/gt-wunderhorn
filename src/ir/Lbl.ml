module QID = QualifiedIdentity

type position = Line of int | Entrance | Exit
type t = At of QID.t * position

let show sep = function
  | At (id, l) ->
    QID.to_string sep (QID.specify id (match l with
        | Line l   -> string_of_int l
        | Entrance -> "Entrance"
        | Exit     -> "Exit"))

let name = show "/"

let qualify prefix = function
  | At (qid, lt) -> At (QID.qualify prefix qid, lt)

let next = function
  | At (id, Line l) -> At (id, Line (l+1))
  | _ -> assert false

let map_ln f a =
  match a with
  | At (id, (Line la)) -> At (id, (Line (f la)))
  | _ -> a

let compare_lines default f a b =
  match (a, b) with
  | (At (_, (Line la)), At (_, (Line lb))) -> f la lb
  | _ -> default

(* checks a > b, must be part of the same program *)
let is_after a b = compare_lines false (>) a b

let is_before a b = compare_lines false (<) a b

