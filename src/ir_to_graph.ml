module E = Expr
module G = Graph
module PG = Program_graph
module LS = Lang_state

(** To inspect the type of an object, the global class array is accessed
    and the contents under the object are compared to the expected value. *)
let check_type obj t =
  E.mk_eq (E.ArrSelect (E.Var LS.class_array, obj)) t

let rec instr (this, next, i) =
  let conditional here there pred assigns =
    G.singleton (here, there, PG.Body (pred, assigns)) in

  let unconditional here there assigns =
    conditional here there E.True assigns in

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
    let assignments = List.map2 E.mk_assign proc.Ir.params args in
    let pc = (proc.Ir.id, E.Int) in
    let entr = proc.Ir.entrance in
    let exit = proc.Ir.exit in

    G.unions
      [ conditional this entr pred ((pc, E.Int_lit this) :: assigns @ assignments)
      ; conditional exit next (E.mk_eq (E.Var pc) (E.Int_lit this)) [(v, (E.Var proc.Ir.return))]
      ; translate proc
      ]
  in

  match i with
    | Ir.Assign (v, e)          -> linear [E.mk_assign v e]
    | Ir.ArrAssign (arr, i, e)  -> linear [LS.update_arr arr i e]
    | Ir.FieldAssign (f, i, e)  -> linear [E.mk_assign f (E.FieldStore (f, i ,e))]
    | Ir.Invoke (p, v, args)    -> call E.True [] p v args
    | Ir.Return (d, v, e)       -> unconditional this d [(v, e)]
    | Ir.Goto d                 -> jump d
    | Ir.New (p, v, ct, es)     -> call E.True (LS.build_object v ct) p v (E.Var v :: es)
    | Ir.NewArray (v, ct, es)   ->
      linear (LS.build_object v ct @ [LS.update_arr LS.array_length (E.Var v) (List.hd es)])

    (** An if statement generates a graph with two edges diverging from one
        starting node. One edge proceeds to the target destination if the
        condition is true. The other proceeds to the next location if the
        condition is false *)
    | Ir.If (cmp, x, y, dest) ->
      G.of_conns
        [ (this, dest, PG.Body (cmp x y, []))
        ; (this, next, PG.Body (E.mk_not (cmp x y), []))
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

and translate proc =
  List.map instr proc.Ir.content
  |> G.unions
