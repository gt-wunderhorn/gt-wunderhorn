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
