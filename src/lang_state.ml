module L = Lang

let rec show_sort = function
  | L.Bool     -> "Bool"
  | L.Int      -> "Int"
  | L.Real     -> "Real"
  | L.String   -> "String"
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

let update_arr arr idx e =
  let store = L.ArrStore (L.Var arr, idx, e) in
  L.mk_assign arr store

(** Building a new object has a few steps.
    1. If the global identity counter does not exist yet, it is created.
    2. If the counter already existed, it is incremented from its previous value.
    3. The object's variable is assigned the value of the counter.
    4. The type of the new object is stored in the global class array. *)
let build_object v ct =
  [ L.mk_assign id (L.mk_add (L.Var id) (L.Int_lit 1))
  ; L.mk_assign v (L.Var id)
  ; update_arr class_array (L.Var v) ct
  ]

let setup es =
  L.mk_impl
    (L.mk_eq (L.Var id) (L.Int_lit 0))
    (L.Relation (0, [L.Var id]))
  :: es
