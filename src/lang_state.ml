module E = Expr

let rec show_sort = function
  | E.Bool     -> "Bool"
  | E.Int      -> "Int"
  | E.Real     -> "Real"
  | E.Array s  -> "Array_" ^ show_sort s

let array_array s =
  ("ARRAY" ^ show_sort s, E.Array (E.Array s))

(** The global class array which stores the types of objects. *)
let class_array = ("CLASS_TYPE", E.Array E.Int)

(** The global array length variable which tracks the lengths of each array in
    the program. *)
let array_length = ("ARRAY_LENGTH", E.Array E.Int)

(** The global identity counter which keeps track of how many objects have been
    created. *)
let id = ("ID", E.Int)
let id_0 = E.Var ("ID_0", E.Int)

let mandatory_vars = [id]

let setup es =
  E.mk_impl
    (E.mk_eq id_0 (E.Int_lit 0))
    (E.Relation (0, [id_0]))
  :: es

let update_arr arr idx e =
  let store = E.ArrStore (E.Var arr, idx, e) in
  E.mk_assign arr store

(** Building a new object has a few steps.
    1. If the global identity counter does not exist yet, it is created.
    2. If the counter already existed, it is incremented from its previous value.
    3. The object's variable is assigned the value of the counter.
    4. The type of the new object is stored in the global class array. *)
let build_object v ct =
  [ E.mk_assign id (E.mk_add (E.Var id) (E.Int_lit 1))
  ; E.mk_assign v (E.Var id)
  ; update_arr class_array (E.Var v) ct
  ]
