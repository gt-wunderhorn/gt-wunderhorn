module L = Lang
module G = Graph
module PG = Program_graph
module LS = Lang_state

let update_arr arr idx e =
  let store = L.ArrStore (L.Var arr, idx, e) in
  L.mk_assign arr store

(** Building a new object has a few steps.
    1. If the global identity counter does not exist yet, it is created.
    2. If the counter already existed, it is incremented from its previous value.
    3. The object's variable is assigned the value of the counter.
    4. The type of the new object is stored in the global class array. *)
let build_object v ct =
  [ L.mk_assign LS.id (L.mk_add (L.Var LS.id) (L.Int_lit 1))
  ; L.mk_assign v (L.Var LS.id)
  ; update_arr LS.class_array (L.Var v) ct
  ]

(** To inspect the type of an object, the global class array is accessed
    and the contents under the object are compared to the expected value. *)
let check_type obj t =
  L.mk_eq (L.ArrSelect (L.Var LS.class_array, obj)) t

let rec instr special (this, next, i) =
  let conditional here there pred assigns =
    G.singleton (here, there, PG.Body (pred, assigns)) in

  let unconditional here there assigns =
    conditional here there L.True assigns in

  (** Many instruction types simply proceed to the next program location with
      some instructions on the edge. *)
  let linear instrs = unconditional this next instrs in

  (** One program location indicates another with an empty edge. *)
  let jump lbl = unconditional this lbl [] in

  (** A call has a few important parts:
        1. An edge is generated which contains the assignments of the formal
           parameters of the target procedure to the call arguments.
        2. An edge is generated which assigns the callers assignment variable
           to the return variable of the callee. It also relates the return
           site to the call site. (In other words, it distinguishes between
           the returns of different calls to the same function.)
        3. A separate graph is evaluated for the callee. *)
  let call pred assigns proc v args =
    let assignments = List.map2 L.mk_assign proc.Ir.params args in
    let pc = (proc.Ir.id, L.Int) in
    let entr = proc.Ir.entrance in
    let exit = proc.Ir.exit in

    G.unions
      [ conditional this entr pred ((pc, L.Int_lit this) :: assigns @ assignments)
      ; conditional exit next (L.mk_eq (L.Var pc) (L.Int_lit this)) [(v, (L.Var proc.Ir.return))]
      ; procedure special proc
      ]
  in

  let base (this, next, i) = match i with
    | Ir.Assign (v, e)          -> linear [L.mk_assign v e]
    | Ir.ArrAssign (arr, v, e)  -> linear [update_arr arr v e]
    | Ir.Invoke (p, v, args)    -> call L.True [] p v args
    | Ir.Return (d, v, e)       -> unconditional this d [(v, e)]
    | Ir.Goto d                 -> jump d
    | Ir.New (p, v, ct, es)     -> call L.True (build_object v ct) p v (L.Var v :: es)
    | Ir.NewArray (v, ct, es)   ->
      linear (build_object v ct @ [update_arr LS.array_length (L.Var v) (List.hd es)])

    (** An if statement generates a graph with two edges diverging from one
        starting node. One edge proceeds to the target destination if the
        condition is true. The other proceeds to the next location if the
        condition is false *)
    | Ir.If (cmp, x, y, dest) ->
      G.of_conns
        [ (this, dest, PG.Body (cmp x y, []))
        ; (this, next, PG.Body (L.mk_not (cmp x y), []))
        ]

    (** A dynamic dispatch generates a graph with many edges diverging from
        one node. Each divergent edge is predicated by a check to see which
        version of the dispatch should be used. *)
    | Ir.Dispatch (obj, ps, v, args) ->
      let call_meth (t, proc) =
        call (check_type obj t) [] proc v (obj :: args) in
      G.unions_map call_meth ps

    | Ir.Assert (e, at) ->
      G.union
        (G.singleton (this, -1, PG.Assert (e, at)))
        (unconditional this next [])
  in
  Special.specialize base special (this, next, i)

and procedure special proc =
  List.map (instr special) proc.Ir.content
  |> G.unions
