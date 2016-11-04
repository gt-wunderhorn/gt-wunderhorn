module L = Lang
module PG = Program_graph

module Alias_table = Count_table.Make(
  struct
    type t = L.var
    let compare = compare
  end)

let mk_alias table (name, sort) =
  (name ^ "_" ^ (string_of_int (Alias_table.get table (name, sort))), sort)

let substitute table =
  let rec su = function
    | L.Var v              -> L.Var (mk_alias table v)
    | L.Un_op (o, e)       -> L.Un_op (o, su e)
    | L.Bi_op (o, e1, e2)  -> L.Bi_op (o, su e1, su e2)
    | L.Many_op (o, es)    -> L.Many_op (o, List.map su es)
    | L.ArrSelect (a, i)   -> L.ArrSelect (su a, su i)
    | L.ArrStore (a, i, e) -> L.ArrStore (su a, su i, su e)
    | e                    -> e
  in su

let translate g =
  let table = Alias_table.empty () in

  let assign_to_expr (v, e) =
    (* Calculating the rhs needs to occur before incrementing the variable *)
    let rhs = substitute table e in
    Alias_table.increment table v;
    L.Bi_op (L.Eq, L.Var (mk_alias table v), rhs) in

  let convert_assignments (e, vs) =
    L.mk_and (e :: List.map assign_to_expr vs) in

  PG.map_body convert_assignments g
