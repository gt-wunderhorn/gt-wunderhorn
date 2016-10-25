module J = Sawja_pack.JBir

(** JBir represents such a thing as string constants. However, string constants
    are deceptive: the `String` class is not actually represented as a primitive.
    Here, we will replace String constants with arrays of characters. *)

let const = function
  | `String s ->
    let raw = JB.jstr_raw s in

    let v = J.var 

    J.NewArray (v, `Char, `Int (String.length raw))

  | c -> c

let unop op e = match op with
  | J.Neg bt        -> L.mk_neg e
  | J.Conv c        -> assert false (* TODO *)
  | J.ArrayLength   -> L.ArrSelect (L.Var LS.array_length, e)
  | J.InstanceOf ot -> assert false (* TODO *)
  | J.Cast ot       -> e (** TODO ?? *)

let binop op x y = match op with
  | J.ArrayLoad t -> assert false (* TODO *)
  | J.Add _       -> assert false (* TODO *)
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

let e_replace_string = function
  | J.Const c              -> const c
  | J.Var (t, v)           -> assert false (* TODO *)
  | J.Binop (op, x, y)     -> assert false (* TODO *)
  | J.Unop (op, e)         -> assert false (* TODO *)
  | J.Field (v, cn, fs)    -> assert false (* TODO *)
  | J.StaticField (cn, fs) -> assert false (* TODO *)

let i_replace_string = function
  | J.AffectVar (v, e)                        -> assert false (* TODO *)
  | J.AffectArray (arr, ind, e)               -> assert false (* TODO *)
  | J.AffectField (v, cn, fs, e)              -> assert false (* TODO *)
  | J.AffectStaticField (cn, fs, e)           -> assert false (* TODO *)
  | J.Ifd ((cond, x, y), l)                   -> assert false (* TODO *)
  | J.Return e                                -> assert false (* TODO *)
  | J.New (v, cn, t, es)                      -> assert false (* TODO *)
  | J.NewArray (v, t, es)                     -> assert false (* TODO *)
  | J.InvokeStatic (v, cn, ms, args)          -> assert false (* TODO *)
  | J.InvokeVirtual (v, obj, ck, _, args)     -> assert false (* TODO *)
  | J.InvokeNonVirtual (v, obj, cn, ms, args) -> assert false (* TODO *)
  | J.MonitorEnter _                          -> assert false (* TODO *)
  | J.MonitorExit _                           -> assert false (* TODO *)
  | J.Throw _                                 -> assert false (* TODO *)
  | J.Formula _                               -> assert false (* TODO *)
  | J.Nop                                     -> assert false (* TODO *)
  | J.MayInit _                               -> assert false (* TODO *)
  | J.Check _                                 -> assert false (* TODO *)
