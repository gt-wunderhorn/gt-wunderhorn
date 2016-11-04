module L = Lang
module G = Graph
module PG = Program_graph
module LS = Lang_state

(** To inspect the type of an object, the global class array is accessed
    and the contents under the object are compared to the expected value. *)
let check_type obj t =
  L.mk_eq (L.ArrSelect (L.Var LS.class_array, obj)) t

let rec instr (this, next, i) =
  let unconditional here there assigns =
    G.singleton (here, there, PG.Body (L.True, assigns)) in

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

    G.union
      (G.of_conns
         [ (this, proc.Ir.entrance,
            PG.Body (pred, (pc, L.Int_lit this) :: assigns @ assignments))
         ; (proc.Ir.exit, next,
            PG.Body (L.mk_eq (L.Var pc) (L.Int_lit this), [(v, (L.Var proc.Ir.return))]))
         ])
      (procedure proc)
  in

  match i with
  | Ir.Assign (v, e)          -> linear [L.mk_assign v e]
  | Ir.ArrAssign (arr, v, e)  -> linear [LS.update_arr arr v e]
  | Ir.New (p, v, ct, es)     -> call L.True (LS.build_object v ct) p v (L.Var v :: es)
  | Ir.NewArray (v, ct, es)   ->
    linear (
      LS.build_object v ct @
      [LS.update_arr LS.array_length (L.Var v) (List.hd es)])
  | Ir.Invoke (p, v, args)    -> call L.True [] p v args
  | Ir.Return (d, v, e)       -> unconditional this d [(v, e)]
  | Ir.Goto d                 -> jump d

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

and procedure proc =
  List.map instr proc.Ir.content
  |> G.unions
