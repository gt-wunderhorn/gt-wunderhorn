module QID = QualifiedIdentity
module Option = Core.Std.Option

type t = Mk of QID.t * Type.t

let qualify prefix (Mk (qid, t)) = Mk (QID.qualify prefix qid, t)
let specify (Mk (qid, t)) suffix = Mk (QID.specify qid suffix, t)
let unspecify (Mk (qid, t)) = Mk (QID.unspecify qid, t)

let as_path (Mk (qid, _)) = QID.as_path qid

let basename (Mk (qid, _)) = QID.most_specific qid

let is_bool (Mk (_, t)) = t = Type.Bool

let is_scalar (Mk (_, t)) = Type.is_scalar t

let type_of (Mk (_, t)) = t

let with_type (Mk (qid, _)) t = Mk (qid, t)

let prime_count (Mk (qid, _)) = try
    Some (int_of_string @@ QID.most_specific qid)
  with _ -> None

let default_prime_count var = Option.value (prime_count var) ~default:0

let primed (Mk (qid, t) as var) count = match prime_count var with
  | Some _ -> Mk (QID.specify (QID.unspecify qid) count, t)
  | None -> Mk (QID.specify qid count, t)

let strip_prime var = match prime_count var with
  | Some _ -> unspecify var
  | None -> var

let is_local (Mk (qid, _)) = function
  | (Lbl.At (method_qid, _)) -> method_qid = QID.full_prefix qid

