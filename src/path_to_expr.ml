module L = Lang
module G = Graph
module PG = Program_graph

module Alias_table = Count_table.Make(
  struct
    type t = L.var
    let compare = compare
  end)

let lookup table (name, sort) =
  (name ^ "_" ^ (string_of_int (Alias_table.get table (name, sort))), sort)

let substitute table =
  let rec su = function
    | L.Var v              -> L.Var (lookup table v)
    | L.Un_op (o, e)       -> L.Un_op (o, su e)
    | L.Bi_op (o, e1, e2)  -> L.Bi_op (o, su e1, su e2)
    | L.Many_op (o, es)    -> L.Many_op (o, List.map su es)
    | L.ArrSelect (a, i)   -> L.ArrSelect (su a, su i)
    | L.ArrStore (a, i, e) -> L.ArrStore (su a, su i, su e)
    | e                    -> e
  in su

let translate_conn ((lbl1, vs1), (lbl2, vs2), edge) =
  let table = Alias_table.empty () in
  let substitute = substitute table in

  let vs1' = List.map (lookup table) vs1 in

  let assign_to_expr (v, e) =
    (* Calculating the rhs needs to occur before incrementing the variable *)
    let rhs = substitute e in
    Alias_table.increment table v;
    L.Bi_op (L.Eq, L.Var (lookup table v), rhs) in

  let convert_assignments (e, vs) =
    let e' = substitute e in
    L.mk_and (e' :: List.map assign_to_expr vs) in

  let edge' = match edge with
    | PG.Assert (e, at) -> PG.Assert (substitute e, at)
    | PG.Body b         -> PG.Body (convert_assignments b)
  in

  let vs2' = List.map (lookup table) vs2 in

  ((lbl1, vs1'), (lbl2, vs2'), edge')

let translate g : (L.lbl * L.var list, ((L.expr, L.expr) PG.path)) G.t
  = G.map_conns translate_conn g
