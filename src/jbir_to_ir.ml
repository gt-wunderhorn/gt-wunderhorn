module J = Sawja_pack.JBir
module JB = Javalib_pack.JBasics
module P = Proc
module E = Expr
module LS = Lang_state
module PG = Program_graph

let const = function
  | `ANull    -> E.Int_lit 0
  | `Class _  -> E.Int_lit 0 (* TODO *)
  | `Double f -> E.Real_lit f
  | `Float f  -> E.Real_lit f
  | `Int i    -> E.Int_lit (Int32.to_int i)
  | `Long i   -> E.Int_lit (Int64.to_int i)
  | `String s -> E.Int_lit 0 (* TODO *)

let rec sort = function
  | JB.TBasic t -> (match t with
      | `Bool   -> E.Int
      | `Byte   -> E.Int
      | `Char   -> E.Int
      | `Double -> E.Real
      | `Float  -> E.Real
      | `Int    -> E.Int
      | `Long   -> E.Int
      | `Short  -> E.Int)
  | JB.TObject t -> E.Int

let unop op e = match op with
  | J.Neg bt        -> E.mk_neg e
  | J.Conv c        -> e (* TODO *)
  | J.ArrayLength   -> E.ArrSelect (E.Var LS.array_length, e)
  | J.InstanceOf ot -> E.True (* TODO *)
  | J.Cast ot       -> e (** TODO ?? *)

let binop op x y = match op with
  | J.ArrayLoad t ->
    let array_array = LS.array_array (sort t) in
    let inner_select = E.ArrSelect (E.Var array_array, x) in
    E.ArrSelect (inner_select, y)
  | J.Add _       -> E.mk_add x y
  | J.Sub _       -> E.mk_sub x y
  | J.Mult _      -> E.mk_mul x y
  | J.Div _       -> E.mk_div x y
  | J.Rem _       -> E.mk_rem x y
  | J.IShl        -> E.mk_bshl x y
  | J.IShr        -> E.mk_bashr x y
  | J.IAnd        -> E.mk_band x y
  | J.IOr         -> E.mk_bor x y
  | J.IXor        -> E.mk_bxor x y
  | J.IUshr       -> E.mk_blshr x y
  | J.LShl        -> E.mk_bshl x y
  | J.LShr        -> E.mk_bashr x y
  | J.LAnd        -> E.mk_band x y
  | J.LOr         -> E.mk_bor x y
  | J.LXor        -> E.mk_bxor x y
  | J.LUshr       -> E.mk_blshr x y
  | J.CMP _       -> assert false (* TODO *)

let field_array_name cn fs = JB.cn_name cn ^ "_" ^ JB.fs_name fs

module Label = Labeller.Make(struct type t = J.var;; let compare = compare end)

type st =
  { parse : Parse.t
  ; proc : Proc.t
  ; labeller : Label.t
  }

let mk_st parse p = { parse = parse; proc = p ; labeller = Label.mk ("v_" ^ p.P.id) }
let rename st = Label.label st.labeller

module S_map = Map.Make(struct type t = string;; let compare = compare end)

let lbl_map = ref S_map.empty
let lbl_count = ref 0

let mk_lbl lbl =
  if not (S_map.mem lbl !lbl_map)
  then
    ( lbl_map := S_map.add lbl !lbl_count !lbl_map
    ; lbl_count := !lbl_count + 1
    );
  S_map.find lbl !lbl_map

let rec expr st = function
  | J.Const c           -> const c
  | J.Var (t, v)        -> E.Var (rename st v, sort t)
  | J.Binop (op, x, y)  -> binop op (expr st x) (expr st y)
  | J.Unop (op, e)      -> unop op (expr st e)
  | J.Field (v, cn, fs) ->
    E.ArrSelect (E.Var
                   ( field_array_name cn fs
                   , E.Array (sort (JB.fs_type fs)))
                , expr st v)
  | J.StaticField (cn, fs) ->
    E.ArrSelect (E.Var
                   ( field_array_name cn fs
                   , E.Array (sort (JB.fs_type fs)))
                , E.Int_lit (st.parse.Parse.class_id cn))

let rec comp cond x y = match cond with
  | `Eq -> E.Bi_op (E.Eq, x, y)
  | `Ge -> E.Bi_op (E.Ge, x, y)
  | `Gt -> E.Bi_op (E.Gt, x, y)
  | `Le -> E.Bi_op (E.Le, x, y)
  | `Lt -> E.Bi_op (E.Lt, x, y)
  | `Ne -> E.Un_op (E.Not, (E.Bi_op (E.Eq, x, y)))

let ctor_sig cn t =
  let ms = JB.make_ms "<init>" t None in
  JB.make_cms cn ms

let dummy = ("DUMMY", E.Int)

let rec ir_proc parse st =
  let p = st.proc in
  { Ir.id       = p.P.id
  ; Ir.entrance = mk_lbl (p.P.id ^ "0")
  ; Ir.exit     = mk_lbl (p.P.id ^ "RET")
  ; Ir.params   = List.map (fun (t, v) -> (rename st v, sort t)) p.P.params
  ; Ir.return   = (p.P.id ^ "RETVAR", Option.map_default sort E.Int p.P.ret_type)
  ; Ir.content  = List.mapi (instr parse st) p.P.content
  }

and mk_proc parse cms =
  let p = parse.Parse.cms_lookup cms in
  ir_proc parse (mk_st parse (List.hd p))

and instr parse st line i =
  let var v s = (rename st v, s) in
  let expr = expr st in
  let e_sort e = E.sort_of (expr e) in
  let field cn fs = (field_array_name cn fs, E.Array (sort (JB.fs_type fs))) in
  let id = st.proc.P.id in
  let lbl n = mk_lbl (id ^ string_of_int n) in

  let this = lbl line in
  let next = lbl (line+1) in

  let return_var v ms =
    match (v, JB.ms_rtype ms) with
    | (Some v, Some s) -> var v (sort s)
    | _ -> dummy
  in

  let invoke cn ms v args =
    match Built_in.call_built_in_method cn ms v args next with
    | Some i -> i
    | None ->
      let proc = mk_proc parse (JB.make_cms cn ms) in
      Ir.Invoke (proc, v, args) in

  let i = match i with
    | J.AffectVar (v, e) ->
      Ir.Assign (var v (e_sort e), expr e)

    | J.AffectArray (arr, ind, e) ->
      let array_array = LS.array_array (e_sort e) in
      let sub_array = E.ArrSelect (E.Var array_array, expr arr) in
      Ir.ArrAssign (array_array, expr arr, E.ArrStore (sub_array, expr ind, expr e))

    | J.AffectField (v, cn, fs, e) ->
      Ir.ArrAssign (field cn fs, expr v, expr e)

    | J.AffectStaticField (cn, fs, e) ->
      Ir.ArrAssign (field cn fs, E.Int_lit (parse.Parse.class_id cn), expr e)

    | J.Goto l ->
      Ir.Goto (lbl l)

    | J.Ifd ((cond, x, y), l) ->
      Ir.If (comp cond, expr x, expr y, lbl l)

    | J.Return e ->
      (** If there is no return parameter, see if there were arguments. If so,
          return the first parameter (which handles constructors). *)
      let backup_ret =
        if List.length st.proc.P.params = 0
        then E.Int_lit 0
        else E.Var ((fun (t, v) -> (rename st v, sort t)) (List.hd st.proc.P.params))
      in

      let e = Option.map_default expr backup_ret e in
      let v = (id ^ "RETVAR", E.sort_of e) in
      Ir.Return (mk_lbl (id ^ "RET"), v, e)

    | J.New (v, cn, t, es) ->
      if Built_in.is_built_in_class cn
      then Ir.Goto next
      else
        let proc = mk_proc parse (ctor_sig cn t) in
        Ir.New (proc, var v E.Int, E.Int_lit (parse.Parse.class_id cn), List.map expr es)

    | J.NewArray (v, t, es) ->
      Ir.NewArray (var v E.Int, E.Int_lit (-1), List.map expr es)

    | J.InvokeStatic (v, cn, ms, args) ->
      let args = List.map expr args in
      let v = return_var v ms in
      invoke cn ms v args

    | J.InvokeNonVirtual (v, obj, cn, ms, args) ->
      let args = List.map expr args in
      let v = return_var v ms in
      invoke cn ms v (expr obj :: args)

    | J.InvokeVirtual (v, obj, ck, ms, args) ->
      let cn = st.proc.P.cl_name in
      let args = List.map expr args in
      let v = return_var v ms in
      (match Built_in.call_built_in_method cn ms v args next with
       | Some i -> i
       | None ->
         let lookup =
           parse.Parse.cms_lookup (JB.make_cms st.proc.P.cl_name ms)
           @
           parse.Parse.virtual_lookup cn st.proc.P.sign line in
         let procs =
           lookup
           |> List.map
             (fun p ->
                ((E.Int_lit (parse.Parse.class_id p.P.cl_name)), ir_proc parse (mk_st parse p))) in
         Ir.Dispatch (expr obj, procs, v, args))

    | J.MayInit cn ->
      let ms = JB.clinit_signature in
      let cms = JB.make_cms cn ms in

      if Built_in.is_built_in_class cn
      || not (parse.Parse.has_cms cms)
      then Ir.Goto next
      else
        let proc = mk_proc parse cms in
        let v = dummy in
        Ir.Invoke (proc, v, [])

    | J.Check c ->
      (match c with
       | J.CheckArrayBound (a, i) ->
         Ir.Assert
           ( E.mk_lt (expr i) (E.ArrSelect (E.Var (LS.array_length), expr a))
           , PG.ArrayBound
           )
       | J.CheckArithmetic e ->
         Ir.Assert (E.mk_not (E.mk_eq (expr e) (E.Int_lit 0)) , PG.Div0)

       | J.CheckNegativeArraySize e ->
         Ir.Assert (E.mk_ge (expr e) (E.Int_lit 0), PG.NegArray)

       | J.CheckNullPointer _
       | J.CheckArrayStore _
       | J.CheckCast _
       | J.CheckLink _
         -> Ir.Goto next)

    | J.MonitorEnter _
    | J.MonitorExit _
    | J.Throw _
    | J.Formula _
    | J.Nop
      -> Ir.Goto next

  in (this, next, i)
