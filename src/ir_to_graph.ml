module L = Lang
module G = L.PG

let id_initialized = ref false

let class_array = ("CLASS_TYPE", L.Array L.Int)
let id = ("ID", L.Int)

let update_arr arr idx e =
  let store = L.ArrStore (L.Var arr, idx, e) in
  L.mk_assign arr store

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

let check_type obj t =
  L.Call (L.mk_eq (L.ArrSelect (L.Var class_array, obj)) t)

let rec show_sort = function
  | L.Bool -> "Bool"
  | L.Int  -> "Int"
  | L.Real -> "Real"
  | L.Array s  -> "Array_" ^ show_sort s
  | L.Object s -> "Object_" ^ show_sort s

let rec instr (this, next, i) =
  let linear instr = G.singleton (this, next, instr) in

  let jump lbl = G.singleton (this, lbl, []) in

  let call pred proc v args =
    let assignments = List.map2 L.mk_assign proc.Ir.params args in
    G.union
      (G.of_list
         [ (this, proc.Ir.entrance, assignments)
         ; (proc.Ir.exit, next,
            pred @ [ L.Relate this
                   ; L.mk_assign v (L.Var proc.Ir.return) ] ) ])
      (procedure proc)
  in

  let g = match i with
    | Ir.Assign (v, e)          -> linear [L.mk_assign v e]
    | Ir.FieldAssign (fa, v, e) -> linear [update_arr fa v e]
    | Ir.New (v, ct, es)        -> linear (build_object v ct)
    | Ir.Invoke (p, v, args)    -> call [] p v args
    | Ir.Return (d, v, e)       -> G.singleton (this, d, [L.mk_assign v e])
    | Ir.Goto d                 -> jump d

    | Ir.ArrAssign (arr, ind, e) ->
      let array_array =
        ( show_sort (L.Object (L.expr_sort e))
        , L.Array (L.Array (L.expr_sort e))) in
      let sub_array = L.ArrSelect (L.Var array_array, arr) in
      linear [update_arr array_array arr (L.ArrStore (sub_array, ind, e))]

    | Ir.If (cmp, x, y, d) ->
      G.of_list
        [ (this, d, [L.Call (cmp x y)])
        ; (this, next, [L.Call (L.mk_not (cmp x y))])
        ]

    | Ir.Dispatch (obj, ps, v, args) ->
      let call_meth (t, proc) =
        call [check_type obj t] proc v (obj :: args) in
      G.unions_map call_meth ps

    | Ir.Assert e ->
      G.of_list
        [ (this, "NOWHERE", [(L.Assert e)])
        ; (this, next, [])
        ]
  in

  let extra_assertions =
    Ir.ir_exprs i
    |> List.map L.expr_assertions
    |> List.concat
  in

  G.union
    (G.unions_map (fun a -> G.singleton (this, "NOWHERE", [a])) extra_assertions)
    g

and procedure proc =
  List.map instr proc.Ir.content
  |> G.unions
  |> G.simplify (@)
