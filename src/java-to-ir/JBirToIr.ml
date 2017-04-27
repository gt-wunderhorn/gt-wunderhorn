module J = Sawja_pack.JBir
module JB = Javalib_pack.JBasics

module P = Proc
module LS = LangState
module QID = QualifiedIdentity

module Set = Core.Std.Set.Poly
module Map = Core.Std.Map.Poly

module T = Type
module E = Expr
module I = Instr
module V = Var

let is_global = function
  | (Var.Mk (QID.QID ("g" :: _), _)) -> true
  | _ -> false

let is_global_assignment = function
  | E.Var var when (is_global var) -> true
  | _ -> false

let expr_replacement replacer is =
  let ex = E.map replacer in
  let replace_vars = function
    | I.Assign (v, e)           -> I.Assign (v, ex e)
    | I.Goto l                  -> I.Goto l
    | I.If (e, l)               -> I.If (ex e, l)
    | I.Return e                -> I.Return (ex e)
    | I.Invoke (p, v, es)       -> I.Invoke (p, v, List.map ex es)
    | I.Assert (e, at)          -> I.Assert (ex e, at)
  in
  List.map
    (fun (I.Instr (lbl, i)) -> I.Instr (lbl, replace_vars i))
    is

let rec gotos_reaching target min max = function
  | (I.Instr (from, I.Goto towards)) :: _
    when (Lbl.compare_lines false (=) towards target)
      && ((Lbl.compare_lines false (<) from min)
         || (Lbl.compare_lines false (>) from max)) ->
    true
  | (I.Instr (from, I.If (_, towards))) :: _
    when (Lbl.compare_lines false (=) towards target)
      && ((Lbl.compare_lines false (<) from min)
         || (Lbl.compare_lines false (>) from max)) ->
    true
  | i :: rest -> gotos_reaching target min max rest
  | [] -> false

(* Some low level languages (including JVM Bytecode) do not represent booleans
   directly. This results in strange clauses. This transforms instructions
   sequences which conditionally assign integers to a boolean assignment. *)
let simplify_boolean_assignment is =
  let rec replace_instrs = function
    | (I.Instr (l1, I.If (c, ll1)))
      :: (I.Instr (l2, I.Assign (v1, E.Int 1)))
      :: (I.Instr (l3, I.Goto ll2))
      :: (I.Instr (l4, I.Assign (v2, E.Int 0)))
      :: (I.Instr (l5, ins))
      :: rest when ll1 = l4 &&
                   ll2 = l5 &&
                   (* from the Java Docs on variable naming:
                      the dollar sign character, by convention, is never used at all.
                      You may find some situations where auto-generated names will
                      contain the dollar sign, but your variable names should always
                      avoid using it.
                      must be compiled with '-g' though *)
                   (Var.basename v1).[0] = '$' &&
                   v1 = v2 ->
      let (m, rest) = replace_instrs (I.Instr ((l5, ins)) :: rest) in
      let v = Var.with_type v1 T.Bool in
      let m' = Map.add ~key:v1 ~data:v m in
      (m',
           (I.Instr (l1, I.If (c, l4)))
        :: (I.Instr (l2, I.Assign (v, E.Bool true)))
        :: (I.Instr (l3, I.Goto l5))
        :: (I.Instr (l4, I.Assign (v, E.Bool false)))
        :: rest)
    | (i :: is) ->
      let (m, rest) = replace_instrs is in
      (m, i :: rest)
    | [] -> (Map.empty, [])
  in

  let (m, is') = replace_instrs is in

  let lookup v = match Map.find m v with
    | None -> v
    | Some v' -> v' in


  let ex =
    let special = function
      | E.Var v -> Some (E.Var (lookup v))
      | e -> None in
    E.map special
  in

  let replace_vars = function
    | I.Assign (V.Mk (var, T.Bool), E.Int (0 | 1 as bin)) ->
      let to_bool = if bin = 1 then true else false in
      I.Assign (V.Mk (var, T.Bool), E.Bool (to_bool))
    | I.Assign (v, e)           -> I.Assign (lookup v, ex e)
    | I.Goto l                  -> I.Goto l
    | I.If (e, l)               -> I.If (ex e, l)
    | I.Return e                -> I.Return (ex e)
    | I.Invoke (p, v, es)       -> I.Invoke (p, lookup v, List.map ex es)
    | I.Assert (e, at)          -> I.Assert (ex e, at)
  in
  List.map
    (fun (I.Instr (lbl, i)) -> I.Instr (lbl, replace_vars i)) is'


(* changes (my_bool = true) -> (my_bool) *)
let simplify_boolean_checks is =
  let replacer = function
    | E.Apply (E.Biop (E.Eq), [E.Var (V.Mk (_, T.Bool)) as var; E.Int (0 | 1 as bin)]) ->
      if bin = 1
      then Some (var)
      else Some (E.Apply (E.Unop (E.Not), [var]))
    | _ -> None
  in
  expr_replacement replacer is

let inline_assignments is =

  let rec slice_linear min max = function
    (* TODO is checking for jumps going _out_ of the region necessary? *)
    (* | ((I.Instr (_, I.Goto towards)) as curr_instr) :: rest *)
    (*   when (Lbl.compare_lines false (<) towards min) *)
    (*     || (Lbl.compare_lines false (>) towards max) -> *)
    (*   ([], curr_instr :: rest) *)
    (* | ((I.Instr (_, I.If (_, towards))) as curr_instr) :: rest *)
    (*   when (Lbl.compare_lines false (<) towards min) *)
    (*     || (Lbl.compare_lines false (>) towards max) -> *)
    (*   ([], curr_instr :: rest) *)
    | ((I.Instr (lbl, _)) as curr_instr) :: rest ->
      if gotos_reaching lbl min max is
      then ([], curr_instr :: rest)
      else
        let (sliced, leftover) = slice_linear min max rest in
        (curr_instr :: sliced, leftover)
    | [] -> ([], [])
  in

  let into_linear_region region = match region with
    | (I.Instr (lbl, _)) :: rest ->
      let min = Lbl.map_ln ((+) (-1)) lbl in
      let max = Lbl.map_ln ((+) (List.length region)) lbl in
      slice_linear min max region
    | [] -> ([], [])
  in

  let rec find_region var dep_vars = function
    | ((I.Instr (lbl, I.Assign (assigned, _))) as curr_instr) :: rest
      when (Set.mem dep_vars assigned) -> ([], curr_instr :: rest)
    | i :: rest ->
      let (region, leftover) = find_region var dep_vars rest in
      (i :: region, leftover)
    | [] -> ([], [])
  in
  let process_region var dep_vars assignment intrs =
    let (region, leftover) = find_region var dep_vars intrs in
    let (linear_region, slice_leftover) = into_linear_region region in
    let rec replacer = function
      | E.Var v when v = var -> Some(assignment)
      | _ -> None
    in
    (expr_replacement replacer linear_region, List.flatten [slice_leftover; leftover])
  in
  let rec find_regions = function
    | (I.Instr (lbl, I.Assign (var, assign))) :: rest
      when (V.is_local var lbl)
        && (V.is_scalar var)
        && (not (is_global_assignment assign)) ->
      let dep_vars = E.vars assign in
      let curr_instr = I.Instr (lbl, I.Assign (var, assign)) in
      if Set.mem dep_vars var
      then curr_instr :: find_regions rest
      else
        let (inlined, leftover) = process_region var (Set.add dep_vars var) assign rest in
        curr_instr :: (find_regions (List.flatten [inlined; leftover]))
    | (i :: rest) -> i :: (find_regions rest)
    | [] -> []
  in
  find_regions is

let remove_unused_vars is =
  let check_used var = function
    | E.Var v when v = var -> Some(true)
    | _ -> None
  in
  let is_used_in_expr var expr = E.fold (check_used var) (||) false expr in

  let rec slice_linear min max = function
    | ((I.Instr (_, I.Goto towards)) as curr_instr) :: rest
      when (Lbl.compare_lines false (<) towards min)
        || (Lbl.compare_lines false (>) towards max) ->
      ([], curr_instr :: rest)
    | ((I.Instr (_, I.If (_, towards))) as curr_instr) :: rest
      when (Lbl.compare_lines false (<) towards min)
        || (Lbl.compare_lines false (>) towards max) ->
      ([], curr_instr :: rest)
    | ((I.Instr (lbl, _)) as curr_instr) :: rest ->
      if gotos_reaching lbl min max is
      then ([], curr_instr :: rest)
      else
        let (sliced, leftover) = slice_linear min max rest in
        (curr_instr :: sliced, leftover)
    | [] -> ([], [])
  in

  let into_linear_region region = match region with
    | (I.Instr (lbl, _)) :: rest ->
      let min = Lbl.map_ln ((+) (-1)) lbl in
      let max = Lbl.map_ln ((+) (List.length region)) lbl in
      slice_linear min max region
    | [] -> ([], [])
  in

  let rec is_used var acc = function
    | (I.Instr (_, (I.Assign (v, e)))) :: _ when is_used_in_expr var e -> true
    | (I.Instr (_, (I.If (e, j))))     :: _ when is_used_in_expr var e -> true
    | (I.Instr (_, (I.Return e)))      :: _ when is_used_in_expr var e -> true
    | (I.Instr (_, (I.Assert (e, _)))) :: _ when is_used_in_expr var e -> true
    | (I.Instr (_, (I.Invoke (_, _, es)))) :: _
      when List.exists (is_used_in_expr var) es       -> true
    | (I.Instr (lbl, (I.Assign (v, _)))) :: _ when var = v ->
      let (_, leftover) = into_linear_region (List.rev acc) in
      leftover != []
    | i :: rest -> is_used var (i :: acc) rest
    | [] ->
      let (used, leftover) = into_linear_region (List.rev acc) in
      leftover != []
  in

  let rec find_unused = function
    | (I.Instr (lbl, I.Assign (var, assign)) as curr_instr)
      :: (I.Instr (next, _) as next_instr)
      :: rest
      when (V.is_local var lbl)
        && (V.is_scalar var)
        && (not (is_global_assignment assign)) ->
      if is_used var [curr_instr] (next_instr :: rest)
      then curr_instr :: (find_unused (next_instr :: rest))
      else (I.Instr (lbl, I.Goto next)) :: (find_unused (next_instr :: rest))
    | (i :: rest) -> i :: (find_unused rest)
    | [] -> []
  in

  find_unused is


let const = function
  | `ANull    -> E.Int 0
  | `Class _  -> E.Int 0 (* TODO *)
  | `Double f -> E.Real f
  | `Float f  -> E.Real f
  | `Int i    -> E.Int (Int32.to_int i)
  | `Long i   -> E.Int (Int64.to_int i)
  | `String s -> E.Int 0 (* TODO *)

let rec typ = function
  | JB.TBasic t -> (match t with
      | `Bool   -> T.Bool
      | `Byte   -> T.Int
      | `Char   -> T.Int
      | `Double -> T.Real
      | `Float  -> T.Real
      | `Int    -> T.Int
      | `Long   -> T.Int
      | `Short  -> T.Int)
  | JB.TObject t -> T.Int


let unop op e = match op with
  | J.Neg bt -> (match bt with
      | `Double | `Float -> E.mk_rneg e
      | `Long            -> E.mk_ineg e
      | `Int2Bool        -> assert false) (* Cannot take negative value of bool *)
  | J.Conv c        -> e (* TODO *)
  | J.ArrayLength   -> E.Select (E.Var LS.array_length, e)
  | J.InstanceOf ot -> E.Bool true (* TODO *)
  | J.Cast ot       -> e (** TODO ?? *)

let binop op x y =
  let num_op t iop rop = match t with
    | `Double | `Float    -> rop x y
    | `Long   | `Int2Bool -> iop x y in
  match op with
  | J.ArrayLoad t ->
    let array_array = LS.array_array (typ t) in
    let inner_select = E.Select (E.Var array_array, x) in
    E.Select (inner_select, y)
  | J.Add  t -> num_op t E.mk_iadd E.mk_radd
  | J.Sub  t -> num_op t E.mk_isub E.mk_rsub
  | J.Mult t -> num_op t E.mk_imul E.mk_rmul
  | J.Div  t -> num_op t E.mk_idiv E.mk_rdiv
  | J.Rem  t -> num_op t E.mk_irem E.mk_rrem
  | J.IShl   -> E.mk_bshl x y
  | J.IShr   -> E.mk_bashr x y
  | J.IAnd   -> E.mk_band x y
  | J.IOr    -> E.mk_bor x y
  | J.IXor   -> E.mk_bxor x y
  | J.IUshr  -> E.mk_blshr x y
  | J.LShl   -> E.mk_bshl x y
  | J.LShr   -> E.mk_bashr x y
  | J.LAnd   -> E.mk_band x y
  | J.LOr    -> E.mk_bor x y
  | J.LXor   -> E.mk_bxor x y
  | J.LUshr  -> E.mk_blshr x y
  | J.CMP _  -> assert false (* TODO *)

let field_name cn fs = QID.of_list [JB.cn_name cn; JB.fs_name fs]
let field cn fs = Var.Mk (field_name cn fs, T.Array (typ (JB.fs_type fs)))

type st =
  { parse : Parse.t
  ; proc : Proc.t
  }

let mk_st parse p = { parse = parse; proc = p }

let names_to_var p_name v_name =
  QID.specify p_name v_name

let rec expr st mk_var jbir = match jbir with
  | J.Const c           -> const c
  | J.Var (t, v)        -> E.Var (mk_var (typ t) v)
  | J.Binop (op, x, y)  -> binop op (expr st mk_var x) (expr st mk_var y)
  | J.Unop (op, e)      -> unop op (expr st mk_var e)
  | J.Field (v, cn, fs) -> E.Select (E.Var (field cn fs), expr st mk_var v)
  | J.StaticField (cn, fs) ->
    E.Select (E.Var (field cn fs), E.Int (st.parse.Parse.class_id cn))

let rec comp cond x y =
  let decide i r =
    match E.type_of x with
    | T.Int  -> i
    | T.Real -> r
    | _ -> assert false
  in
  match cond with
  | `Eq -> E.mk_eq x y
  | `Ge -> decide E.mk_ige E.mk_rge x y
  | `Gt -> decide E.mk_igt E.mk_rgt x y
  | `Le -> decide E.mk_ile E.mk_rle x y
  | `Lt -> decide E.mk_ilt E.mk_rlt x y
  | `Ne -> E.mk_not (E.mk_eq x y)

let ctor_sig cn t =
  let ms = JB.make_ms "<init>" t None in
  JB.make_cms cn ms

let update_field arr idx e =
  let store = E.Store (E.Var arr, idx, e) in
  I.Assign (arr, store)

(* How many instructions are inserted for each original instruction? *)
let number_of_new_instructions parse st line = function
  | J.InvokeVirtual (v, obj, ck, ms, args) ->
    let cn = st.proc.P.cl_name in
    (if List.mem (JB.ms_name ms) BuiltIn.built_in_list
     then 1
     else
       let procs =
         parse.Parse.cms_lookup (JB.make_cms st.proc.P.cl_name ms)
         @ parse.Parse.virtual_lookup cn st.proc.P.sign line in
       3 * List.length procs)
  | J.NewArray _ -> 4
  | _ -> 1

let rec ir_proc parse st cn =
  let p = st.proc in
  let module OT = OffsetTable in

  let opening_instructions =
    let name = JB.ms_name st.proc.Proc.sign in
    if name = "main"
    then
      [I.Assign (LS.id, E.Int 0)]
    else if name = "<init>"
    then
      let this = Var.Mk (QID.specify p.P.name "this", T.Int) in
      let class_t = E.Int (parse.Parse.class_id cn) in
      [ I.Assign (LS.id, E.mk_iadd (E.Var LS.id) (E.Int 1))
      ; I.Assign (this, E.Var LS.id)
      ; update_field LS.class_array (E.Var this) class_t
      ]
    else
      [] in

  let vartable = p.P.vartable in
  let find_type_from_table var_name = match vartable with
    | None -> None
    | Some (locals) ->
      (* TODO: Multiple vars of the same name within function?? *)
      (* TODO: the same thing for feild names *)
      let same_name (_, _, s, _, _) = s = (J.var_name_g var_name) in
      let vars = List.filter (same_name) locals in
      match vars with
        | ((_, _, _, t, _) :: _) -> Some (typ t)
        | _ -> None
  in

  (* TODO: Check how this works with non-local variables *)
  let var_name st v = names_to_var st.proc.P.name (J.var_name_g v) in
  let mk_var t v =
    let better_type = find_type_from_table v in
    Var.Mk (var_name st v, Option.default t better_type)
  in

  let instrs = Lazy.from_fun (fun _ ->
      let jump_offsets =
        List.mapi (number_of_new_instructions parse st) p.P.content
        |> List.mapi (fun line num -> (line, num - 1))
        |> List.fold_left (fun tb (l, o) -> OT.add (l+1) o tb)
           (OT.mk (List.length opening_instructions)) in

      let renumber = function
        | J.Goto l       -> J.Goto (l + OT.lookup jump_offsets l)
        | J.Ifd (cmp, l) -> J.Ifd (cmp, l + OT.lookup jump_offsets l)
        | other          -> other
      in

      let groups =
        p.P.content
        |> List.map renumber
        |> List.mapi (function line -> instr parse st mk_var line (line + OT.lookup jump_offsets line)) in

      opening_instructions :: groups
      |> List.concat
      |> List.mapi (fun line ir -> I.Instr (Lbl.At (p.P.name, Lbl.Line line), ir))
      |> simplify_boolean_assignment
      |> simplify_boolean_checks
      |> inline_assignments
      |> remove_unused_vars
    ) in

  { I.id       = p.P.name
  ; I.params   = List.map (fun (t, v) -> mk_var (typ t) v) p.P.params
  ; I.ret_type = Core.Std.Option.value_map ~default:T.Unit ~f:typ p.P.ret_type
  ; I.content  = instrs
  }

and mk_proc parse cms =
  let p = parse.Parse.cms_lookup cms in
  ir_proc parse (mk_st parse (List.hd p)) (fst (JB.cms_split cms))

and instr parse st mk_var orig_line line i =
  let expr = expr st mk_var in
  let e_type e = E.type_of (expr e) in
  let fname = QID.as_path st.proc.P.name in

  let src_line = match J.get_source_line_number orig_line st.proc.P.j_method with
    | None   -> -1
    | Some n -> n in

  let lbl l = Lbl.At (st.proc.P.name, Lbl.Line l) in
  let next = lbl (line+1) in

  let return_var v ms =
    match (v, JB.ms_rtype ms) with
    | (Some v, Some s) -> mk_var (typ s) v
    | _ -> LS.dummy
  in

  let invoke cn ms v args =
    match BuiltIn.call_built_in_method fname src_line cn ms v args next with
    | Some i -> i
    | None ->
      let proc = mk_proc parse (JB.make_cms cn ms) in
      I.Invoke (proc, v, args)
  in

  match i with
  | J.AffectVar (v, e) ->
    [I.Assign (mk_var (e_type e) v, expr e)]

  | J.AffectArray (arr, ind, e) ->
    let array_array = LS.array_array (e_type e) in
    let sub_array = E.Select (E.Var array_array, expr arr) in
    [update_field array_array (expr arr) (E.Store (sub_array, expr ind, expr e))]

  | J.AffectField (v, cn, fs, e) ->
    [update_field (field cn fs) (expr v) (expr e)]

  | J.AffectStaticField (cn, fs, e) ->
    [update_field (field cn fs) (E.Int (parse.Parse.class_id cn)) (expr e)]

  | J.Goto l ->
    [I.Goto (lbl l)]

  | J.Ifd ((cond, x, y), l) ->
    [I.If (comp cond (expr x) (expr y), lbl l)]

  | J.Return e ->
    (** If there is no return parameter, see if there were arguments. If so,
        return the first parameter (which handles constructors). *)

    if QID.most_specific (st.proc.Proc.name) = "<init>"
    then
      [I.Return (E.Var LS.id)]
    else
      let backup_ret =
        if List.length st.proc.P.params = 0
        then E.Int 0
        else E.Var ((fun (t, v) -> mk_var (typ t) v) (List.hd st.proc.P.params))
      in
      let e = Option.map_default expr backup_ret e in
      [I.Return e]

  | J.New (v, cn, t, es) ->
    if BuiltIn.is_built_in_class cn
    then [I.Goto next]
    else
      let proc = mk_proc parse (ctor_sig cn t) in
      [I.Invoke (proc, mk_var T.Int v, List.map expr es)]

  | J.NewArray (v, t, es) ->
    let v' = mk_var T.Int v in
    [ I.Assign (LS.id, E.mk_iadd (E.Var LS.id) (E.Int 1))
    ; I.Assign (v', E.Var LS.id)
    ; update_field LS.class_array (E.Var v') (E.Int (-1))
    ; update_field LS.array_length (E.Var v') (expr (List.hd es))
    ]

  | J.InvokeStatic (v, cn, ms, args) ->
    let args = List.map expr args in
    let v = return_var v ms in
    [invoke cn ms v args]

  | J.InvokeNonVirtual (v, obj, cn, ms, args) ->
    let args = List.map expr args in
    let v = return_var v ms in
    [invoke cn ms v (expr obj :: args)]

  | J.InvokeVirtual (v, obj, ck, ms, args) ->
    let cn = st.proc.P.cl_name in
    let args = List.map expr args in
    let v = return_var v ms in
    (match BuiltIn.call_built_in_method fname src_line cn ms v args next with
     | Some i -> [i]
     | None ->
       let procs =
         parse.Parse.cms_lookup (JB.make_cms st.proc.P.cl_name ms)
         @ parse.Parse.virtual_lookup cn st.proc.P.sign orig_line in

       let final_dest = lbl (line + 3*(List.length procs)) in

       procs
       |> List.mapi
         (fun idx p ->
            let cid = E.Int (parse.Parse.class_id p.P.cl_name) in
            let dest = lbl (line + 3*(idx + 1)) in
            let _ = mk_proc parse (JB.make_cms p.P.cl_name p.P.sign) in

            [ I.If (E.mk_neq cid (E.Select (E.Var LS.class_array, expr obj)), dest)
            ; invoke p.P.cl_name p.P.sign v (expr obj :: args)
            ; I.Goto final_dest
            ])
       |> List.concat)

  | J.MayInit cn ->
    let ms = JB.clinit_signature in
    let cms = JB.make_cms cn ms in

    if BuiltIn.is_built_in_class cn || not (parse.Parse.has_cms cms)
    then [I.Goto next]
    else
      let proc = mk_proc parse cms in
      let v = LS.dummy in
      [I.Invoke (proc, v, [])]

  | J.Check c ->
  (match c with
   | J.CheckArrayBound (a, i) ->
     [I.Assert
       ( E.mk_ilt (expr i) (E.Select (E.Var (LS.array_length), expr a))
       , E.QueryInfo (Assert.ArrayBound, fname, src_line)
       )]

   | J.CheckArithmetic e ->
     [I.Assert
        ( E.mk_not (E.mk_eq (expr e) (E.Int 0))
        , E.QueryInfo (Assert.Div0, fname, src_line))]

   | J.CheckNegativeArraySize e ->
     [I.Assert
        ( E.mk_ige (expr e) (E.Int 0)
        , E.QueryInfo (Assert.NegArray, fname, src_line))]

   | J.CheckNullPointer _
   | J.CheckArrayStore _
   | J.CheckCast _
   | J.CheckLink _
     -> [I.Goto next])

  | J.MonitorEnter _
  | J.MonitorExit _
  | J.Throw _
  | J.Formula _
  | J.Nop
    -> [I.Goto next]
