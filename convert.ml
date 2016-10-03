module A3 = Sawja_pack.A3Bir
module JB = Javalib_pack.JBasics
module P = Program

let var v = P.Variable (A3.var_name v)

let tvar (_, v) = var v
let tvar_e v = P.Var (tvar v)

let const = function
  | `ANull    -> assert false (* TODO *)
  | `Class _  -> assert false (* TODO *)
  | `Double f -> assert false (* TODO *)
  | `Float f  -> assert false (* TODO *)
  | `Int i    -> P.Int_lit (Int32.to_int i)
  | `Long i   -> assert false (* TODO *)
  | `String s -> assert false (* TODO *)

let expr = function
  | A3.Const c       -> const c
  | A3.Var v         -> assert false (* TODO *)
  | A3.Unop _        -> assert false (* TODO *)
  | A3.Binop _       -> assert false (* TODO *)
  | A3.Field _       -> assert false (* TODO *)
  | A3.StaticField _ -> assert false (* TODO *)

let rec compare cond x y = match cond with
  | `Eq -> P.Eq (tvar_e x, tvar_e y)
  | `Ge -> assert false (* TODO *)
  | `Gt -> assert false (* TODO *)
  | `Le -> assert false (* TODO *)
  | `Lt -> assert false (* TODO *)
  | `Ne -> P.Not (compare `Eq x y)

let convert is =
  let offset = ref 0 in
  let instr = function
    | A3.Nop                   -> offset := !offset + 1; []
    | A3.AffectVar (v, e)      -> [P.Linear (P.Assign (var v, expr e))]
    | A3.AffectArray _         -> assert false (* TODO *)
    | A3.AffectField _         -> assert false (* TODO *)
    | A3.AffectStaticField _   -> assert false (* TODO *)
    | A3.Goto l                -> [P.Non_linear (P.Goto (l - !offset))]
    | A3.Ifd ((cond, x, y), l) -> [P.Non_linear (P.If (compare cond x y, l - !offset))]
    | A3.Throw _               -> assert false (* TODO *)
    | A3.Return _              -> [P.Linear (P.Return (P.Int_lit 0))] (* TODO *)
    | A3.New _                 -> assert false (* TODO *)
    | A3.NewArray _            -> assert false (* TODO *)
    | A3.InvokeStatic (_, _, ms, vs) ->
      if (JB.ms_name ms) = "ensure"
      then [P.Linear (P.Assert (tvar (List.hd vs)))]
      else assert false (* TODO *)
    | A3.InvokeVirtual _       -> assert false (* TODO *)
    | A3.InvokeNonVirtual _    -> assert false (* TODO *)
    | A3.MonitorEnter _        -> assert false (* TODO *)
    | A3.MonitorExit _         -> assert false (* TODO *)
    | A3.MayInit _             -> offset := !offset + 1; []
    | A3.Check _               -> assert false (* TODO *)
    | A3.Formula _             -> assert false (* TODO *)
  in
  List.map instr is |> List.concat
