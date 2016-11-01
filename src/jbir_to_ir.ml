module J = Sawja_pack.JBir
module JB = Javalib_pack.JBasics
module P = Proc
module L = Lang
module LS = Lang_state

let const = function
  | `ANull    -> L.Int_lit 0
  | `Class _  -> L.Int_lit 0 (* TODO *)
  | `Double f -> L.Real_lit f
  | `Float f  -> L.Real_lit f
  | `Int i    -> L.Int_lit (Int32.to_int i)
  | `Long i   -> L.Int_lit (Int64.to_int i)
  | `String s -> L.Int_lit 0 (* TODO *)

let rec sort = function
  | JB.TBasic t -> (match t with
      | `Bool   -> L.Int
      | `Byte   -> assert false (* TODO *)
      | `Char   -> L.Int
      | `Double -> L.Real
      | `Float  -> L.Real
      | `Int    -> L.Int
      | `Long   -> L.Int
      | `Short  -> L.Int)
  | JB.TObject t -> L.Int

let unop op e = match op with
  | J.Neg bt        -> L.mk_neg e
  | J.Conv c        -> e (* TODO *)
  | J.ArrayLength   -> L.ArrSelect (L.Var LS.array_length, e)
  | J.InstanceOf ot -> L.True (* TODO *)
  | J.Cast ot       -> e (** TODO ?? *)

let binop op x y = match op with
  | J.ArrayLoad t ->
    let array_array = LS.array_array (sort t) in
    let inner_select = L.ArrSelect (L.Var array_array, x) in
    L.ArrSelect (inner_select, y)
  | J.Add _       -> L.mk_add x y
  | J.Sub _       -> L.mk_sub x y
  | J.Mult _      -> L.mk_mul x y
  | J.Div _       -> L.mk_div x y
  | J.Rem _       -> L.mk_rem x y
  | J.IShl        -> L.mk_bshl x y
  | J.IShr        -> L.mk_bashr x y
  | J.IAnd        -> L.mk_band x y
  | J.IOr         -> L.mk_bor x y
  | J.IXor        -> L.mk_bxor x y
  | J.IUshr       -> L.mk_blshr x y
  | J.LShl        -> L.mk_bshl x y
  | J.LShr        -> L.mk_bashr x y
  | J.LAnd        -> L.mk_band x y
  | J.LOr         -> L.mk_bor x y
  | J.LXor        -> L.mk_bxor x y
  | J.LUshr       -> L.mk_blshr x y
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

let class_initialized = ref []

let mk_lbl lbl =
  if not (S_map.mem lbl !lbl_map)
  then
    ( lbl_map := S_map.add lbl !lbl_count !lbl_map
    ; lbl_count := !lbl_count + 1
    )
  ;
  S_map.find lbl !lbl_map

let rec expr st = function
  | J.Const c           -> const c
  | J.Var (t, v)        -> L.Var (rename st v, sort t)
  | J.Binop (op, x, y)  -> binop op (expr st x) (expr st y)
  | J.Unop (op, e)      -> unop op (expr st e)
  | J.Field (v, cn, fs) ->
    L.ArrSelect (L.Var
                   ( field_array_name cn fs
                   , L.Array (sort (JB.fs_type fs)))
                , expr st v)
  | J.StaticField (cn, fs) ->
    L.ArrSelect (L.Var
                   ( field_array_name cn fs
                   , L.Array (sort (JB.fs_type fs)))
                , L.Int_lit (st.parse.Parse.class_id cn))

let rec comp cond x y = match cond with
  | `Eq -> L.Bi_op (L.Eq, x, y)
  | `Ge -> L.Bi_op (L.Ge, x, y)
  | `Gt -> L.Bi_op (L.Gt, x, y)
  | `Le -> L.Bi_op (L.Le, x, y)
  | `Lt -> L.Bi_op (L.Lt, x, y)
  | `Ne -> L.Un_op (L.Not, (L.Bi_op (L.Eq, x, y)))

let ctor_sig cn t =
  let ms = JB.make_ms "<init>" t None in
  JB.make_cms cn ms

let rec ir_proc parse st =
  let p = st.proc in
  { Ir.id       = p.P.id
  ; Ir.entrance = mk_lbl (p.P.id ^ "0")
  ; Ir.exit     = mk_lbl (p.P.id ^ "RET")
  ; Ir.params   = List.map (fun (t, v) -> (rename st v, sort t)) p.P.params
  ; Ir.return   = (p.P.id ^ "RETVAR", Option.map_default sort L.Int p.P.ret_type)
  ; Ir.content  = List.mapi (instr parse st) p.P.content
  }

and mk_proc parse cms =
  let p = parse.Parse.cms_lookup cms in
  ir_proc parse (mk_st parse (List.hd p))

and instr parse st line i =
  let var v s = (rename st v, s) in
  let expr = expr st in
  let e_sort e = L.expr_sort (expr e) in
  let id = st.proc.P.id in
  let lbl n = mk_lbl (id ^ string_of_int n) in

  let this = lbl line in
  let next = lbl (line+1) in

  let return_var proc =
    Option.map_default (fun v -> var v (snd proc.Ir.return)) ("DUMMY", L.Int)
  in

  let built_in name v args =
    let arbitrary t =
      Some (match v with
          | Some v ->
            let v = var v t in
            Ir.Assign (v, L.Var v)
          | None -> Ir.Goto next) in

    if name = "ensure"
    then Some (Ir.Assert (L.mk_eq (List.hd args) (L.Int_lit 1), L.User))
    else if name = "hasNextShort"
         || name = "hasNextInt"
         || name = "hasNextLong"
         || name = "hasNextBigInteger"
         || name = "hasNextFloat"
         || name = "hasNextDouble"
    then arbitrary L.Bool
    else if name = "nextShort"
         || name = "nextInt"
         || name = "nextLong"
         || name = "nextBigInteger"
    then arbitrary L.Int
    else if name = "nextFloat"
         || name = "nextDouble"
    then arbitrary L.Real
    else if name = "print"
         || name = "println"
         || name = "close"
         || name = "getClass"
         || name = "getComponentType"
         || name = "desiredAssertionStatus"
         || name = "getPrimitiveClass"
         || name = "getSavedProperty"
    then Some (Ir.Goto next)
    else if name = "valueOf"
    then
      let v' = Option.map_default (fun v -> var v L.Int) ("DUMMY", L.Int) v in
      Some (Ir.Assign (v', List.hd args))
    else None
  in

  let is_built_in_class cn =
    let name = JB.cn_name cn in
    name = "java.util.Scanner" ||
    name = "java.util.Properties" ||
    name = "java.io.BufferedInputStream" ||
    name = "java.lang.Integer" ||
    name = "java.lang.System" ||
    name = "java.lang.Object" ||
    name = "java.lang.Class" ||
    name = "sun.misc.VM"
  in

  let i = match i with
    | J.AffectVar (v, e) -> Ir.Assign (var v (e_sort e), expr e)

    | J.AffectArray (arr, ind, e) ->
      let array_array = LS.array_array (e_sort e) in
      let sub_array = L.ArrSelect (L.Var array_array, expr arr) in
      Ir.ArrAssign (array_array, expr arr, L.ArrStore (sub_array, expr ind, expr e))

    | J.AffectField (v, cn, fs, e) ->
      let fa = (field_array_name cn fs, L.Array (e_sort e)) in
      Ir.ArrAssign (fa, expr v, expr e)

    | J.AffectStaticField (cn, fs, e) ->
      let fa = (field_array_name cn fs, L.Array (e_sort e)) in
      Ir.ArrAssign (fa, L.Int_lit (parse.Parse.class_id cn), expr e)

    | J.Goto l ->
      Ir.Goto (lbl l)

    | J.Ifd ((cond, x, y), l) ->
      Ir.If (comp cond, expr x, expr y, lbl l)

    | J.Return e ->
      (** If there is no return parameter, see if there were arguments. If so,
          return the first parameter (which handles constructors). *)
      let backup_ret =
        if List.length st.proc.P.params = 0
        then L.Int_lit 0
        else L.Var ((fun (t, v) -> (rename st v, sort t)) (List.hd st.proc.P.params))
      in

      let e = Option.map_default expr backup_ret e in
      let v = (id ^ "RETVAR", L.expr_sort e) in
      Ir.Return (mk_lbl (id ^ "RET"), v, e)

    | J.New (v, cn, t, es) ->
      Printf.eprintf "new %s\n" (JB.cn_name cn);

      if is_built_in_class cn
      then Ir.Goto next
      else
        let proc = mk_proc parse (ctor_sig cn t) in
        Ir.New (proc, var v L.Int, L.Int_lit (parse.Parse.class_id cn), List.map expr es)

    | J.NewArray (v, t, es) ->
      Ir.NewArray (var v L.Int, L.Int_lit (-1), List.map expr es)

    | J.InvokeStatic (v, cn, ms, args) ->
      Printf.eprintf "static %s\n" (JB.ms_name ms);
      let args = List.map expr args in
      (match built_in (JB.ms_name ms) v args with
       | Some i -> i
       | None ->
         let proc = mk_proc parse (JB.make_cms cn ms) in
         let v = return_var proc v in
         Ir.Invoke (proc, v, args))

    | J.InvokeVirtual (v, obj, ck, ms, args) ->
      Printf.eprintf "virtual %s %s %d\n" (JB.ms_name ms) (JB.cn_name st.proc.P.cl_name) line;
      let args = List.map expr args in

      let lookup =
        parse.Parse.cms_lookup (JB.make_cms st.proc.P.cl_name ms)
        @
        parse.Parse.virtual_lookup st.proc.P.sign line in
      (match built_in (JB.ms_name ms) v args with
       | Some i -> i
       | None ->
         let procs =
           lookup
           |> List.map
             (fun p ->
                ((L.Int_lit (parse.Parse.class_id p.P.cl_name)), ir_proc parse (mk_st parse p))) in
         let v = return_var (snd (List.hd procs)) v in
         Ir.Dispatch (expr obj, procs, v, args))

    | J.InvokeNonVirtual (v, obj, cn, ms, args) ->
      Printf.eprintf "nonvirtual %s %s\n" (JB.cn_name cn) (JB.ms_name ms);

      if (JB.cn_name cn) = "java.lang.Throwable"
      then Ir.Goto next
      else
        let proc = mk_proc parse (JB.make_cms cn ms) in
        let args = List.map expr args in

        (match built_in (JB.ms_name ms) v args with
         | Some i -> i
         | None ->
           let v = return_var proc v in
           Ir.Invoke (proc, v, (expr obj) :: args))

    | J.MayInit cn ->
      let ms = JB.clinit_signature in
      let cms = JB.make_cms cn ms in


      Printf.eprintf "initializing %s %d\n" (JB.cn_name cn) (List.length !class_initialized);
      if is_built_in_class cn
      || List.mem cn !class_initialized
      || not (parse.Parse.has_cms cms)
      then Ir.Goto next
      else
        ( Printf.eprintf "initializing %s\n" (JB.cn_name cn);
          class_initialized := cn :: !class_initialized;
          let proc = mk_proc parse cms in
          let v = ("DUMMY", L.Int) in
          Ir.Invoke (proc, v, [])
        )

    | J.MonitorEnter _
    | J.MonitorExit _
    | J.Throw _
    | J.Formula _
    | J.Nop
      -> Ir.Goto next

    | J.Check c ->
      (match c with
       | J.CheckArrayBound (a, i) ->
         Ir.Assert
           ( L.mk_lt (expr i) (L.ArrSelect (L.Var (LS.array_length), expr a))
           , L.ArrayBound
           )
       | J.CheckArithmetic e ->
         Ir.Assert (L.mk_not (L.mk_eq (expr e) (L.Int_lit 0)) , L.Div0)

       | J.CheckNegativeArraySize e ->
         Ir.Assert (L.mk_ge (expr e) (L.Int_lit 0), L.NegArray)

       | J.CheckNullPointer _
       | J.CheckArrayStore _
       | J.CheckCast _
       | J.CheckLink _
         -> Ir.Goto next)

  in (this, next, i)

let procedure parse p =
  List.mapi (instr parse (mk_st parse p)) p.P.content
