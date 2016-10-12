module J = Sawja_pack.JBir
module JB = Javalib_pack.JBasics
module L = Lang

type instr_state =
  { st : Proc.t
  ; mutable extra_instrs : L.instr list
  }

let const = function
  | `ANull    -> L.Int_lit 0
  | `Class _  -> assert false (* TODO *)
  | `Double f -> L.Real_lit f
  | `Float f  -> L.Real_lit f
  | `Int i    -> L.Int_lit (Int32.to_int i)
  | `Long i   -> L.Int_lit (Int64.to_int i)
  | `String s -> assert false (* TODO *)

let binop st op x y = match op with
  | J.ArrayLoad _ ->
    let array_array = ("ARRAY", L.Array (L.Array L.Int)) in
    L.ArrSelect (L.ArrSelect (L.Var array_array, x), y)
  | J.Add _       -> L.mk_add x y
  | J.Sub _       -> assert false (* TODO *)
  | J.Mult _      -> L.mk_mul x y
  | J.Div _       ->
    st.extra_instrs <-
      (L.Assert (L.mk_not (L.mk_eq y (L.Int_lit 0)))) :: st.extra_instrs;
    L.mk_div x y
  | J.Rem _       -> assert false (* TODO *)
  | J.IShl        -> assert false (* TODO *)
  | J.IShr        -> assert false (* TODO *)
  | J.IAnd        -> assert false (* TODO *)
  | J.IOr         -> assert false (* TODO *)
  | J.IXor        -> assert false (* TODO *)
  | J.IUshr       -> assert false (* TODO *)
  | J.LShl        -> assert false (* TODO *)
  | J.LShr        -> assert false (* TODO *)
  | J.LAnd        -> assert false (* TODO *)
  | J.LOr         -> assert false (* TODO *)
  | J.LXor        -> assert false (* TODO *)
  | J.LUshr       -> assert false (* TODO *)
  | J.CMP _       -> assert false (* TODO *)

let field_array_name cn fs = JB.cn_name cn ^ "_" ^ JB.fs_name fs

let rec expr st = function
  | J.Const c -> const c
  | J.Var (t, v) -> L.Var (Proc.var st.st v (Proc.sort t))
  | J.Binop (op, x, y)  -> binop st op (expr st x) (expr st y)
  | J.Unop _            -> assert false (* TODO *)
  | J.Field (v, cn, fs) ->
    L.ArrSelect (L.Var (field_array_name cn fs, L.Array L.Int), expr st v)
  | J.StaticField _     -> assert false (* TODO *)

let rec comp st cond x y = match cond with
  | `Eq -> L.Bi_op (L.Eq, expr st x, expr st y)
  | `Ge -> L.Bi_op (L.Ge, expr st x, expr st y)
  | `Gt -> L.Bi_op (L.Gt, expr st x, expr st y)
  | `Le -> L.Bi_op (L.Le, expr st x, expr st y)
  | `Lt -> L.Bi_op (L.Lt, expr st x, expr st y)
  | `Ne -> L.Un_op (L.Not, (L.Bi_op (L.Eq, expr st x, expr st y)))

let opposite = function
  | `Eq -> `Ne
  | `Ge -> `Lt
  | `Gt -> `Le
  | `Le -> `Gt
  | `Lt -> `Ge
  | `Ne -> `Eq

let id_initialized = ref false

let rec instr parse proc line instr =
  let id = proc.Proc.id in
  let mk_lbl n = id ^ string_of_int n in
  let this = mk_lbl line in
  let next = mk_lbl (line+1) in
  let noop _ = L.PG.singleton (this, next, []) in

  let st =
    { st = proc
    ; extra_instrs = []
    } in

  let build_identity v =
    let id = ("ID", L.Int) in
    let id_init = if !id_initialized
      then []
      else
        (id_initialized := true;
         [L.Assign (id, L.Int_lit 1)]) in

    L.PG.singleton
      (this, next,
       id_init @
       [ L.Assign (id, L.mk_add (L.Var id) (L.Int_lit 1))
       ; L.Assign (Proc.var st.st v L.Int, L.Var id)
       ]) in

  let g = match instr with
    | J.Nop -> noop ()
    | J.AffectVar (v, e) ->
      let ex = expr st e in
      L.PG.singleton
        (this, next,
         [L.Assign (Proc.var st.st v (L.expr_sort ex), ex)])
    | J.AffectArray (arr, ind, e) ->
      let ex = expr st e in
      let array_array =
        ("ARRAY", L.Array (L.Array (L.expr_sort ex))) in (* TODO, array type *)

      let sub_array =
        L.ArrSelect (L.Var array_array, expr st arr) in

      L.PG.singleton
        (this, next,
         [L.Assign (array_array
                   , L.ArrStore (
                       L.Var array_array,
                       expr st arr,
                       L.ArrStore (sub_array, expr st ind, ex)))])

    | J.AffectField (v, cn, fs, e) ->
      let field_array = (field_array_name cn fs, L.Array L.Int) in (* TODO, array type *)

      L.PG.singleton
        (this, next,
         [L.Assign (field_array , L.ArrStore (L.Var field_array, expr st v, expr st e))])
    | J.AffectStaticField _ -> assert false
    | J.Goto l -> L.PG.singleton (this, mk_lbl l, [])
    | J.Ifd ((cond, x, y), l) ->
      L.PG.of_list
        [ (this, mk_lbl l, [L.Call (comp st cond x y)])
        ; (this, next, [L.Call (comp st (opposite cond) x y)])
        ]
    | J.Throw _ -> assert false
    | J.Return e ->
      L.PG.singleton
        (let lbl = id ^ "RET" in
         match e with
         | None   -> (this, lbl, [])
         | Some e ->
           let ex = expr st e in
           let retvar = (id ^ "RETVAR", (L.expr_sort ex)) in
           (this, lbl, [L.Assign (retvar, ex)]))
    | J.New (v, cn, t, es) -> build_identity v
    | J.NewArray (v, t, es) -> build_identity v
    | J.InvokeStatic (v, cn, ms, args) ->
      if (JB.ms_name ms) = "ensure"
      then L.PG.of_list
          [ (this, "NOWHERE", [(L.Assert (L.mk_eq (expr st (List.hd args)) (L.Int_lit 1)))])
          ; (this, next, [])
          ]
      else
        let proc = (parse.Parse.cms_lookup (JB.make_cms cn ms)) in
        let sort = match proc.Proc.ret_type with
          | None -> L.Int
          | Some t -> Proc.sort t in
        let v = match v with
          | None -> ("DUMMY", L.Int)
          | Some v -> Proc.var st.st v sort in
        let assignments = List.map2
            (fun param arg -> L.Assign (param, expr st arg))
            (List.map (fun (t, p) -> Proc.var proc p (Proc.sort t)) proc.Proc.params)
            args in
        let ret = proc.Proc.id ^ "RET" in
        let retvar = (proc.Proc.id ^ "RETVAR", sort) in

        let proc_graph = convert parse proc in
        L.PG.union
          (L.PG.of_list
             [ (this, proc.Proc.id ^ "0", assignments)
             ; (ret, next,
                [ L.Relate this
                ; L.Assign (v, L.Var retvar) ]) ])
          proc_graph

    | J.InvokeVirtual (v, e, ck, ms, es) -> assert false (* TODO *)
    | J.InvokeNonVirtual _    -> assert false (* TODO *)
    | J.MonitorEnter _        -> assert false (* TODO *)
    | J.MonitorExit _         -> assert false (* TODO *)
    | J.MayInit _ -> noop ()
    | J.Check _               -> noop ()
    | J.Formula _             -> assert false (* TODO *)

  in
  if List.length st.extra_instrs > 0
  then
    let extra_instrs = L.PG.singleton (this, "NOWHERE", st.extra_instrs) in
    L.PG.union extra_instrs g
  else
    g

and convert parse proc =
  List.mapi (instr parse proc) proc.Proc.content
  |> L.PG.unions
  |> L.PG.simplify (@)
