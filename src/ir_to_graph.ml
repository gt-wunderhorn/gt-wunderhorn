module L = Lang
module G = L.PG

let id_initialized = ref false

(** The global class array which stores the types of objects. *)
let class_array = ("CLASS_TYPE", L.Array L.Int)
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
  let id_init =
    if !id_initialized
    then []
    else (id_initialized := true;
          [L.mk_assign id (L.Int_lit 1)]) in

  id_init @
  [ L.mk_assign id (L.mk_add (L.Var id) (L.Int_lit 1))
  ; L.mk_assign v (L.Var id)
  ; update_arr class_array (L.Var v) ct
  ]

(** To inspect the type of an object, the global class array is accessed
    and the contents under the object are compared to the expected value. *)
let check_type obj t =
  L.Call (L.mk_eq (L.ArrSelect (L.Var class_array, obj)) t)

let rec instr (this, next, i) =
  (** Many instruction types simply proceed to the next program location with
      some instructions on the edge. *)
  let linear instrs = G.singleton (this, next, instrs) in

  (** One program location indicates another with an empty edge. *)
  let jump lbl = G.singleton (this, lbl, []) in

  (** A call has a few important parts:
        1. An edge is generated which contains the assignments of the formal
           parameters of the target procedure to the call arguments.
        2. An edge is generated which assigns the callers assignment variable
           to the return variable of the callee. It also relates the return
           site to the call site. (In other words, it distinguishes between
           the returns of different calls to the same function.)
        3. A separate graph is evaluated for the callee. *)
  let call pred proc v args =
    let assignments = List.map2 L.mk_assign proc.Ir.params args in

    let pc = (proc.Ir.id, L.Int) in

    G.union
      (G.of_list
         [ (this, proc.Ir.entrance,
            L.Assign (pc, L.Int_lit this) :: pred @ assignments)
         ; (proc.Ir.exit, next,
            [ L.Call (L.mk_eq (L.Var pc) (L.Int_lit this))
            ; L.mk_assign v (L.Var proc.Ir.return)
            ] )
         ])
      (procedure proc)
  in

  let g = match i with
    | Ir.Assign (v, e)          -> linear [L.mk_assign v e]
    | Ir.ArrAssign (arr, v, e)  -> linear [update_arr arr v e]
    | Ir.New (p, v, ct, es)     -> call (build_object v ct) p v (L.Var v :: es)
    | Ir.NewArray (v, ct, es)   ->
      linear (

        update_arr array_length (L.Var v) (List.hd es) ::
        build_object v ct)
    | Ir.Invoke (p, v, args)    -> call [] p v args
    | Ir.Return (d, v, e)       -> G.singleton (this, d, [L.mk_assign v e])
    | Ir.Goto d                 -> jump d

    (** An if statement generates a graph with two edges diverging from one
        starting node. One edge proceeds to the target destination if the
        condition is true. The other proceeds to the next location if the
        condition is false *)
    | Ir.If (cmp, x, y, d) ->
      G.of_list
        [ (this, d, [L.Call (cmp x y)])
        ; (this, next, [L.Call (L.mk_not (cmp x y))])
        ]

    (** A dynamic dispatch generates a graph with many edges diverging from
        one node. Each divergent edge is predicated by a check to see which
        version of the dispatch should be used. *)
    | Ir.Dispatch (obj, ps, v, args) ->
      let call_meth (t, proc) =
        call [check_type obj t] proc v (obj :: args) in
      G.unions_map call_meth ps

    | Ir.Assert e ->
      G.of_list
        [ (this, -1, [(L.Assert e)])
        ; (this, next, [])
        ]
  in

  let extra_assertions =
    Ir.ir_exprs i
    |> List.map L.expr_assertions
    |> List.concat
  in

  G.union
    (G.unions_map (fun a -> G.singleton (this, -1, [a])) extra_assertions)
    g

and procedure proc =
  List.map instr proc.Ir.content
  |> G.unions
