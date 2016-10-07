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
