module L = Lang

let rec show_sort = function
  | L.Bool     -> "Bool"
  | L.Int      -> "Int"
  | L.Real     -> "Real"
  | L.Array s  -> "Array_" ^ show_sort s

let array_array s =
  ("ARRAY" ^ show_sort s, L.Array (L.Array s))

(** The global class array which stores the types of objects. *)
let class_array = ("CLASS_TYPE", L.Array L.Int)

(** The global array length variable which tracks the lengths of each array in
    the program. *)
let array_length = ("ARRAY_LENGTH", L.Array L.Int)

(** The global identity counter which keeps track of how many objects have been
    created. *)
let id = ("ID", L.Int)
let id_0 = L.Var ("ID_0", L.Int)

let mandatory_vars = [id]

let setup es =
  L.mk_impl
    (L.mk_eq id_0 (L.Int_lit 0))
    (L.Relation (0, [id_0]))
  :: es
