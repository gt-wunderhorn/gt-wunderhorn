module J = Sawja_pack.JBir
module JB = Javalib_pack.JBasics
module P = Proc
module L = Lang

let const = function
  | `ANull    -> L.Int_lit 0
  | `Class _  -> assert false (* TODO *)
  | `Double f -> L.Real_lit f
  | `Float f  -> L.Real_lit f
  | `Int i    -> L.Int_lit (Int32.to_int i)
  | `Long i   -> L.Int_lit (Int64.to_int i)
  | `String s -> assert false (* TODO *)

let binop op x y = match op with
  | J.ArrayLoad _ ->
    (** TODO getting the correct sort for an array load *)
    let array_array = ("ARRAY", L.Array (L.Array L.Int)) in
    let inner_select = L.ArrSelect (L.Var array_array, x) in
    L.ArrSelect (inner_select, y)
  | J.Add _       -> L.mk_add x y
  | J.Sub _       -> assert false (* TODO *)
  | J.Mult _      -> L.mk_mul x y
  | J.Div _       -> L.mk_div x y
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

let rec sort = function
  | JB.TBasic t -> (match t with
      | `Bool   -> L.Bool
      | `Byte   -> assert false (* TODO *)
      | `Char   -> assert false (* TODO *)
      | `Double -> L.Real
      | `Float  -> L.Real
      | `Int    -> L.Int
      | `Long   -> L.Int
      | `Short  -> L.Int)
  | JB.TObject t -> L.Int

let field_array_name cn fs = JB.cn_name cn ^ "_" ^ JB.fs_name fs

let rec expr st = function
  | J.Const c -> const c
  | J.Var (t, v) -> L.Var (P.var st v, sort t)
  | J.Binop (op, x, y)  -> binop op (expr st x) (expr st y)
  | J.Unop _            -> assert false (* TODO *)
  | J.Field (v, cn, fs) ->
    (** TODO getting the correct type for a field select *)
    L.ArrSelect (L.Var (field_array_name cn fs, L.Array L.Int), expr st v)
  | J.StaticField _     -> assert false (* TODO *)

let rec comp cond x y = match cond with
  | `Eq -> L.Bi_op (L.Eq, x, y)
  | `Ge -> L.Bi_op (L.Ge, x, y)
  | `Gt -> L.Bi_op (L.Gt, x, y)
  | `Le -> L.Bi_op (L.Le, x, y)
  | `Lt -> L.Bi_op (L.Lt, x, y)
  | `Ne -> L.Un_op (L.Not, (L.Bi_op (L.Eq, x, y)))

let rec ir_proc parse p =
  { Ir.entrance = p.P.id ^ "0"
  ; Ir.exit     = p.P.id ^ "RET"
  ; Ir.params   = List.map (fun (t, v) -> (P.var p v, (sort t))) p.P.params
  ; Ir.return   = (p.P.id ^ "RETVAR", Option.map_default sort L.Int p.P.ret_type)
  ; Ir.content  = List.mapi (instr parse p) p.P.content
  }

and mk_proc parse cms =
  let p = (parse.Parse.cms_lookup cms) in
  ir_proc parse p

and instr parse proc line i =
  let var v s = (P.var proc v, s) in
  let expr = expr proc in
  let e_sort e = L.expr_sort (expr e) in
  let id = proc.P.id in
  let mk_lbl n = id ^ string_of_int n in

  let this = mk_lbl line in
  let next = mk_lbl (line+1) in

  let i = match i with
    | J.AffectVar (v, e) -> Ir.Assign (var v (e_sort e), expr e)

    | J.AffectArray (arr, ind, e) ->
      Ir.ArrAssign (expr arr, expr ind, expr e)

    | J.AffectField (v, cn, fs, e) ->
      let fa = (field_array_name cn fs, L.Array (e_sort e)) in
      Ir.FieldAssign (fa, expr v, expr e)

    | J.Goto l ->
      Ir.Goto (mk_lbl l)

    | J.Ifd ((cond, x, y), l) ->
      Ir.If (comp cond, expr x, expr y, mk_lbl l)

    | J.Return e ->
      let e = Option.map_default expr (L.Int_lit 0) e in
      let v = (proc.P.id ^ "RETVAR", L.expr_sort e) in
      Ir.Return (proc.P.id ^ "RET", v, e)

    | J.New (v, cn, t, es) ->
      Ir.New (var v L.Int, L.Int_lit (parse.Parse.class_id cn), List.map expr es)

    | J.NewArray (v, t, es) ->
      Ir.New (var v L.Int, L.Int_lit (-1), List.map expr es)

    | J.InvokeStatic (v, cn, ms, args) ->
      if (JB.ms_name ms) = "ensure"
      then
        Ir.Assert (L.mk_eq (expr (List.hd args)) (L.Int_lit 1))
      else
        let proc = mk_proc parse (JB.make_cms cn ms) in
        let v = Option.map_default (fun v -> var v (snd proc.Ir.return)) ("DUMMY", L.Int) v in
        Ir.Invoke (proc, v, List.map expr args)

    | J.InvokeVirtual (v, obj, ck, _, args) ->
      let procs =
        parse.Parse.virtual_lookup proc.P.sign line
        |> List.map
          (fun p -> ((L.Int_lit (parse.Parse.class_id p.P.cl_name)), ir_proc parse p)) in

      let v =
        if List.length procs = 0
        then ("DUMMY", L.Int)
        else Option.map_default (fun v -> var v (snd (snd (List.hd procs)).Ir.return)) ("DUMMY", L.Int) v in

      Ir.Dispatch (expr obj, procs, v, List.map expr args)

    | J.InvokeNonVirtual _
    | J.MonitorEnter _
    | J.MonitorExit _
    | J.AffectStaticField _
    | J.Throw _
    | J.Formula _
      -> assert false

    | J.Nop
    | J.MayInit _
    | J.Check _
      -> Ir.Goto next

  in (this, next, i)

let procedure parse proc =
  List.mapi (instr parse proc) proc.P.content
