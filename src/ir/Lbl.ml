module QID = QualifiedIdentity

type position = Line of int | Entrance | Exit
type t = At of QID.t * position | Nowhere

let show sep = function
  | At (id, l) ->
    QID.to_string sep (QID.specify id (match l with
        | Line l   -> string_of_int l
        | Entrance -> "Entrance"
        | Exit     -> "Exit"))
  | Nowhere  -> "Nowhere"

let name = show "/"

let qualify prefix = function
  | At (qid, lt) -> At (QID.qualify prefix qid, lt)
  | Nowhere -> Nowhere

let next = function
  | At (id, Line l) -> At (id, Line (l+1))
  | _ -> assert false

let lbl_to_str = function
  | At (qid, (Line lineno)) -> Printf.sprintf "Lbl %s:%d" (QID.as_path qid) lineno
  | At (qid, (Entrance)) -> Printf.sprintf "Lbl %s:Entrance" (QID.as_path qid)
  | At (qid, (Exit)) -> Printf.sprintf "Lbl %s:Exit" (QID.as_path qid)
  | Nowhere -> "Lbl Nowhere"

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

