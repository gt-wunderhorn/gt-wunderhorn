module J = Sawja_pack.JBir
module JB = Javalib_pack.JBasics

let rec sort = function
  | JB.TBasic t -> (match t with
      | `Bool   -> Ir.Bool
      | `Byte   -> assert false (* TODO *)
      | `Char   -> assert false (* TODO *)
      | `Double -> assert false (* TODO *)
      | `Float  -> assert false (* TODO *)
      | `Int    -> Ir.Int
      | `Long   -> Ir.Int
      | `Short  -> Ir.Int)
  | JB.TObject t -> Ir.Int

let var v s = Ir.Variable (J.var_name v, s)

let tvar (s, v) = var v (sort s)
let tvar_e v = Ir.Var (tvar v)

let var_sort (s, _) = sort s

let field_array_name cn fs = JB.cn_name cn ^ "_" ^ JB.fs_name fs

let const = function
  | `ANull    -> assert false (* TODO *)
  | `Class _  -> assert false (* TODO *)
  | `Double f -> assert false (* TODO *)
  | `Float f  -> assert false (* TODO *)
  | `Int i    -> Ir.Int_lit (Int32.to_int i)
  | `Long i   -> assert false (* TODO *)
  | `String s -> assert false (* TODO *)

let binop op x y = match op with
  | J.ArrayLoad _ ->
    let array_array = Ir.Variable ("ARRAY", Ir.Array (Ir.Array Ir.Int)) in
    Ir.ArrSelect (Ir.ArrSelect (Ir.Var array_array, x), y)
  | J.Add _       -> Ir.Add (x, y)
  | J.Sub _       -> assert false (* TODO *)
  | J.Mult _      -> assert false (* TODO *)
  | J.Div _       -> assert false (* TODO *)
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

let rec expr = function
  | J.Const c           -> const c
  | J.Var (t, v)        -> Ir.Var (var v (sort t))
  | J.Binop (op, x, y)  -> binop op (expr x) (expr y)
  | J.Unop _            -> assert false (* TODO *)
  | J.Field (v, cn, fs) ->
    Ir.ArrSelect (Ir.Var (Ir.Variable (field_array_name cn fs, Ir.Array Ir.Int)), expr v)
  | J.StaticField _     -> assert false (* TODO *)

let rec compare cond x y = match cond with
  | `Eq -> Ir.Eq (expr x, expr y)
  | `Ge -> Ir.Ge (expr x, expr y)
  | `Gt -> Ir.Gt (expr x, expr y)
  | `Le -> Ir.Le (expr x, expr y)
  | `Lt -> Ir.Lt (expr x, expr y)
  | `Ne -> Ir.Not (compare `Eq x y)

let rec convert parse is =
  let offset = ref 0 in
  let offsets = ref [] in
  let noop _ = offset := !offset+1; [] in

  let build_identity v =
    let counter = Ir.Variable ("COUNT", Ir.Int) in
    offset := !offset-1;
    [ Ir.Linear (Ir.Assign (counter, Ir.Add (Ir.Var counter, Ir.Int_lit 1)))
    ; Ir.Linear (Ir.Assign (var v Ir.Int, Ir.Var counter))
    ] in

  let instr i =
    offsets := !offsets @ [!offset];

    match i with
    | J.Nop -> noop ()

    | J.AffectVar (v, e)      ->
      let e = expr e in
      [Ir.Linear (Ir.Assign (var v (Ir.expr_sort e), e))]

    | J.AffectArray (arr, ind, e) ->
      let array_array =
        Ir.Variable ("ARRAY", Ir.Array (Ir.Array Ir.Int)) in (* TODO, array type *)

      let sub_array =
        Ir.ArrSelect (Ir.Var array_array, expr arr) in

      [Ir.Linear (Ir.Assign (array_array
                            , Ir.ArrStore (
                                Ir.Var array_array,
                                expr arr,
                                Ir.ArrStore (sub_array, expr ind, expr e))))]

    | J.AffectField (v, cn, fs, e) ->
      let field_array =
        Ir.Variable (field_array_name cn fs, Ir.Array Ir.Int) in (* TODO, array type *)
      [Ir.Linear (Ir.Assign (field_array
                            , Ir.ArrStore (Ir.Var field_array, expr v, expr e)))]

    | J.AffectStaticField _ ->
      assert false (* TODO *)
    | J.Goto l ->
      [Ir.Non_linear (Ir.Goto l)]

    | J.Ifd ((cond, x, y), l) ->
      [Ir.Non_linear (Ir.If (compare cond x y, l))]

    | J.Throw _ ->
      assert false (* TODO *)

    | J.Return v              ->
      let v = match v with
        | None   -> Ir.Int_lit 0
        | Some v -> expr v in
      [Ir.Non_linear (Ir.Return v)]

    | J.New (v, cn, t, es) ->
      build_identity v
      (* TODO, how do I get graph? *)

    | J.NewArray (v, t, es) ->
      build_identity v

    | J.InvokeStatic (v, cn, ms, vs) ->
      if (JB.ms_name ms) = "ensure"
      then [Ir.Linear (Ir.Assert (expr (List.hd vs)))]
      else let v = match v with
          | None   -> Ir.Variable ("DUMMY", Ir.Int)
          | Some v -> var v Ir.Int in (* TODO, sort is wrong *)
        let proc = Ir.map (convert parse) (parse (JB.make_cms cn ms)) in
        [Ir.Non_linear (Ir.Invoke (v, proc, List.map expr vs))]

    | J.InvokeVirtual _       -> assert false (* TODO *)
    | J.InvokeNonVirtual _    -> assert false (* TODO *)
    | J.MonitorEnter _        -> assert false (* TODO *)
    | J.MonitorExit _         -> assert false (* TODO *)
    | J.MayInit _             -> noop ()
    | J.Check _               -> noop ()
    | J.Formula _             -> assert false (* TODO *)
  in

  let account_for_offsets = function
    | Ir.Non_linear (Ir.Goto l)    -> Ir.Non_linear (Ir.Goto (l - (List.nth !offsets l)))
    | Ir.Non_linear (Ir.If (e, l)) -> Ir.Non_linear (Ir.If (e, (l - List.nth !offsets l)))
    | i -> i
  in

  is
  |> List.map instr
  |> List.concat
  |> List.map account_for_offsets
