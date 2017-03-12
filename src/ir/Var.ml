module QID = QualifiedIdentity

type t = Mk of QID.t * Type.t

let qualify prefix (Mk (qid, t)) = Mk (QID.qualify prefix qid, t)
let specify (Mk (qid, t)) suffix = Mk (QID.specify qid suffix, t)

let as_path (Mk (qid, _)) = QID.as_path qid

let basename (Mk (qid, _)) = QID.most_specific qid

let is_bool (Mk (_, t)) = t = Type.Bool

let is_scalar (Mk (_, t)) = Type.is_scalar t

let type_of (Mk (_, t)) = t

let with_type (Mk (qid, _)) t = Mk (qid, t)

let is_local (Mk (qid, _)) = function
  | (Lbl.Nowhere) -> false
  | (Lbl.At (method_qid, _)) -> method_qid = (QID.full_prefix qid)

let var_to_str = function Mk (qid, kind) ->
  Printf.sprintf "Mk (%s, %s)" (QID.as_path qid) (Type.type_to_str kind)
