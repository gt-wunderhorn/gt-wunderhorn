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
  | J.Rem _       -> L.mk_rem x y
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
  | J.Var (t, v) -> L.Var (Proc.var st.st v, Proc.sort t)
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
  let class_array = ("CLASS_TYPE", L.Array L.Int) in

  let st =
    { st = proc
    ; extra_instrs = []
    } in

  let assign name e = L.Assign ((name, L.expr_sort e), e) in
  let store name idx e = L.ArrStore (L.Var (name, L.Array (L.expr_sort e)), idx, e) in
  let linear instr = L.PG.singleton (this, next, [instr]) in
  let update_arr name idx e = assign name (store name idx e) in
  let jump lbl = L.PG.singleton (this, lbl, []) in

  let build_identity v =
    let id_init =
      if !id_initialized
      then []
      else (id_initialized := true;
            [assign "ID" (L.Int_lit 1)]) in

    id_init @
    [ assign "ID" (L.mk_add (L.Var ("ID", L.Int)) (L.Int_lit 1))
    ; assign v (L.Var ("ID", L.Int))
    ] in

  let call pred proc v args =
    let sort = match proc.Proc.ret_type with
      | None -> L.Int
      | Some t -> Proc.sort t in
    let v = match v with
      | None -> "DUMMY"
      | Some v -> Proc.var st.st v in
    let assignments = List.map2
        (fun param arg -> assign param (expr st arg))
        (List.map (fun (t, p) -> Proc.var proc p) proc.Proc.params)
        args in
    let ret = proc.Proc.id ^ "RET" in
    let retvar = (proc.Proc.id ^ "RETVAR", sort) in

    let proc_graph = convert parse proc in
    L.PG.union
      (L.PG.of_list
         [ (this, proc.Proc.id ^ "0", assignments)
         ; (ret, next,
            [ L.Relate this
            ; assign v (L.Var retvar) ] @ pred) ])
      proc_graph
  in

  let ex e = expr st e in

  let g = match instr with
    | J.AffectVar (v, e) -> linear (assign (Proc.var st.st v) (ex e))
    | J.AffectArray (arr, ind, e) ->
      let array_array = ("ARRAY", L.Array (L.Array (L.expr_sort (ex e)))) in

      let sub_array = L.ArrSelect (L.Var array_array, expr st arr) in
      linear (update_arr "ARRAY" (expr st arr) (L.ArrStore (sub_array, expr st ind, (ex e))))

    | J.AffectField (v, cn, fs, e) ->
      let field_array = field_array_name cn fs in
      linear (update_arr field_array (ex v) (ex e))

    | J.AffectStaticField _ -> assert false
    | J.Goto l -> jump (mk_lbl l)
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
         | Some e -> (this, lbl, [assign (id ^ "RETVAR") (ex e)]))

    | J.New (v, cn, t, es) ->
      let v = Proc.var st.st v in
      let instrs =
        build_identity v @
        [update_arr "CLASS_TYPE" (L.Var (v, L.Int)) (L.Int_lit (parse.Parse.class_id cn))]
      in

      L.PG.singleton (this, next, instrs)

    | J.NewArray (v, t, es) ->
      let v = Proc.var st.st v in
      L.PG.singleton (this, next, build_identity v)
    | J.InvokeStatic (v, cn, ms, args) ->
      if (JB.ms_name ms) = "ensure"
      then L.PG.of_list
          [ (this, "NOWHERE", [(L.Assert (L.mk_eq (ex (List.hd args)) (L.Int_lit 1)))])
          ; (this, next, [])
          ]
      else
        let proc = (parse.Parse.cms_lookup (JB.make_cms cn ms)) in
        call [] proc v args

    | J.InvokeVirtual (v, obj, ck, _, args) ->
      let procs = parse.Parse.virtual_lookup proc.Proc.sign line in

      let call_meth proc =
        let pred = L.Call (L.mk_eq
                             (L.ArrSelect (L.Var class_array, ex obj))
                             (L.Int_lit (parse.Parse.class_id proc.Proc.cl_name))) in
        call [pred] proc v (obj :: args)
      in
      L.PG.unions_map call_meth procs

    | J.InvokeNonVirtual _    -> assert false (* TODO *)
    | J.MonitorEnter _        -> assert false (* TODO *)
    | J.MonitorExit _         -> assert false (* TODO *)
    | J.Formula _             -> assert false (* TODO *)
    | J.Nop | J.MayInit _ | J.Check _ -> jump next

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
