module A3 = Sawja_pack.A3Bir
module JB = Javalib_pack.JBasics

let var v = Ir.Variable (A3.var_name v)

let tvar (_, v) = var v
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
  | A3.ArrayLoad _ -> assert false (* TODO *)
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
    | A3.AffectVar (v, e)      -> [Ir.Linear (Ir.Assign (var v, expr e))]
    | A3.AffectArray _         -> assert false (* TODO *)
    | A3.AffectField _         -> assert false (* TODO *)
    | A3.AffectStaticField _   -> assert false (* TODO *)
    | A3.Goto l                -> [Ir.Non_linear (Ir.Goto (l - !offset))]
    | A3.Ifd ((cond, x, y), l) -> [Ir.Non_linear (Ir.If (compare cond x y, l - !offset))]
    | A3.Throw _               -> assert false (* TODO *)
    | A3.Return v              ->
      let v = match v with
        | None   -> Ir.Var (Ir.Variable "DUMMY")
        | Some v -> tvar_e v in
      [Ir.Non_linear (Ir.Return v)]
    | A3.New _                 -> assert false (* TODO *)
    | A3.NewArray _            -> assert false (* TODO *)
    | A3.InvokeStatic (v, cn, ms, vs) ->
      if (JB.ms_name ms) = "ensure"
      then [Ir.Linear (Ir.Assert (tvar (List.hd vs)))]
      else let v = match v with
          | None   -> Ir.Variable "DUMMY"
          | Some v -> var v in
        [Ir.Non_linear (Ir.Invoke (v, JB.make_cms cn ms, List.map tvar_e vs))]
    | A3.InvokeVirtual _       -> assert false (* TODO *)
    | A3.InvokeNonVirtual _    -> assert false (* TODO *)
    | A3.MonitorEnter _        -> assert false (* TODO *)
    | A3.MonitorExit _         -> assert false (* TODO *)
    | A3.MayInit _             -> offset := !offset + 1; []
    | A3.Check _               -> assert false (* TODO *)
    | A3.Formula _             -> assert false (* TODO *)
  in
  List.map instr is |> List.concat
