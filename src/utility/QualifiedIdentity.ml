(** A qualified identity is intended to be a unique identity for some entity.
   The id consists of a list of sub-ids which become increasingly specific as
   the list is traversed from left to right. *)
type t = QID of string list

type store = t list

(** Combine the subidentifiers into a single identifier. Each component is
   delimited by the first argument *)
let to_string delim (QID xs) = String.concat delim xs

let of_list xs = QID xs

let as_path = to_string "/"

let specify (QID qid) to_add = QID (qid @ [to_add])
let unspecify (QID qid) = QID (Algorithm.init qid)

let qualify to_add (QID qid) = QID (to_add :: qid)
let unqualify (QID qid) = match qid with
  | []      -> QID []
  | (x::xs) -> QID xs

let full_prefix (QID qid) = QID (Algorithm.init qid)

let prefix (QID qid) =
  match qid with
  | [] -> QID []
  | (x :: _) -> QID [x]

let most_specific (QID qid) = Algorithm.last qid
