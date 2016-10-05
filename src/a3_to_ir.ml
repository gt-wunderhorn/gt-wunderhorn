module A3 = Sawja_pack.A3Bir
module JB = Javalib_pack.JBasics

let rec sort = function
  | JB.TBasic t -> (match t with
      | `Bool   -> Ir.Bool
      | `Byte   -> assert false (* TODO *)
      | `Char   -> assert false (* TODO *)
      | `Double -> assert false (* TODO *)
      | `Float  -> assert false (* TODO *)
      | `Int    -> Ir.Int
      | `Long   -> assert false (* TODO *)
      | `Short  -> assert false (* TODO *))
  | JB.TObject t -> (match t with
      | JB.TArray t  -> Ir.Array (sort t)
      | JB.TClass cn -> (* TODO *)
        Ir.Array Ir.Int)

let var v s = Ir.Variable (A3.var_name v, s)

let tvar (s, v) = var v (sort s)
let tvar_e v = Ir.Var (tvar v)

let const = function
  | `ANull    -> assert false (* TODO *)
  | `Class _  -> assert false (* TODO *)
  | `Double f -> assert false (* TODO *)
  | `Float f  -> assert false (* TODO *)
  | `Int i    -> Ir.Int_lit (Int32.to_int i)
  | `Long i   -> assert false (* TODO *)
  | `String s -> assert false (* TODO *)

let binop op x y = match op with
  | A3.ArrayLoad _ -> Ir.ArrSelect (x, y)
  | A3.Add _       -> Ir.Add (x, y)
  | A3.Sub _       -> assert false (* TODO *)
  | A3.Mult _      -> assert false (* TODO *)
  | A3.Div _       -> assert false (* TODO *)
  | A3.Rem _       -> assert false (* TODO *)
  | A3.IShl        -> assert false (* TODO *)
  | A3.IShr        -> assert false (* TODO *)
  | A3.IAnd        -> assert false (* TODO *)
  | A3.IOr         -> assert false (* TODO *)
  | A3.IXor        -> assert false (* TODO *)
  | A3.IUshr       -> assert false (* TODO *)
  | A3.LShl        -> assert false (* TODO *)
  | A3.LShr        -> assert false (* TODO *)
  | A3.LAnd        -> assert false (* TODO *)
  | A3.LOr         -> assert false (* TODO *)
  | A3.LXor        -> assert false (* TODO *)
  | A3.LUshr       -> assert false (* TODO *)
  | A3.CMP _       -> assert false (* TODO *)

let expr = function
  | A3.Const c          -> const c
  | A3.Var v            -> tvar_e v
  | A3.Binop (op, x, y) -> binop op (tvar_e x) (tvar_e y)
  | A3.Unop _           -> assert false (* TODO *)
  | A3.Field _          -> assert false (* TODO *)
  | A3.StaticField _    -> assert false (* TODO *)

let rec compare cond x y = match cond with
  | `Eq -> Ir.Eq (tvar_e x, tvar_e y)
  | `Ge -> Ir.Ge (tvar_e x, tvar_e y)
  | `Gt -> Ir.Gt (tvar_e x, tvar_e y)
  | `Le -> Ir.Le (tvar_e x, tvar_e y)
  | `Lt -> Ir.Lt (tvar_e x, tvar_e y)
  | `Ne -> Ir.Not (compare `Eq x y)

let convert is =
  let offset = ref 0 in
  let instr = function
    | A3.Nop                   -> offset := !offset + 1; []
    | A3.AffectVar (v, e)      ->
      let e = expr e in
      [Ir.Linear (Ir.Assign (var v (Ir.expr_sort e), e))]
    | A3.AffectArray (arr, ind, exp) ->
      [Ir.Linear (Ir.Assign
                    ( tvar arr
                    , Ir.ArrStore (tvar arr, tvar_e ind, tvar_e exp)))]
    | A3.AffectField _         -> assert false (* TODO *)
    | A3.AffectStaticField _   -> assert false (* TODO *)
    | A3.Goto l                -> [Ir.Non_linear (Ir.Goto (l - !offset))]
    | A3.Ifd ((cond, x, y), l) -> [Ir.Non_linear (Ir.If (compare cond x y, l - !offset))]
    | A3.Throw _               -> assert false (* TODO *)
    | A3.Return v              ->
      let v = match v with
        | None   -> Ir.Var (Ir.Variable ("DUMMY", Ir.Int))
        | Some v -> tvar_e v in
      [Ir.Non_linear (Ir.Return v)]
    | A3.New _                 -> assert false (* TODO *)
    | A3.NewArray _            -> offset := !offset + 1; [] (* TODO *)
    | A3.InvokeStatic (v, cn, ms, vs) ->
      if (JB.ms_name ms) = "ensure"
      then [Ir.Linear (Ir.Assert (tvar (List.hd vs)))]
      else let v = match v with
          | None   -> Ir.Variable ("DUMMY", Ir.Int)
          | Some v -> var v Ir.Int in (* TODO, sort is wrong *)
        [Ir.Non_linear (Ir.Invoke (v, JB.make_cms cn ms, List.map tvar_e vs))]
    | A3.InvokeVirtual _       -> assert false (* TODO *)
    | A3.InvokeNonVirtual _    -> assert false (* TODO *)
    | A3.MonitorEnter _        -> assert false (* TODO *)
    | A3.MonitorExit _         -> assert false (* TODO *)
    | A3.MayInit _             -> offset := !offset + 1; []
    | A3.Check _               -> offset := !offset + 1; []
    | A3.Formula _             -> assert false (* TODO *)
  in
  List.map instr is |> List.concat
