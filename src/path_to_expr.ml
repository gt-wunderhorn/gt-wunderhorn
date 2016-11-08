module E = Expr
module G = Graph
module PG = Program_graph

module Alias_table = Count_table.Make(
  struct
    type t = E.var
    let compare = compare
  end)

let lookup table (name, sort) =
  (name ^ "_" ^ (string_of_int (Alias_table.get table (name, sort))), sort)

let substitute table =
  let rec su = function
    | E.Var v              -> E.Var (lookup table v)
    | E.Un_op (o, e)       -> E.Un_op (o, su e)
    | E.Bi_op (o, e1, e2)  -> E.Bi_op (o, su e1, su e2)
    | E.Many_op (o, es)    -> E.Many_op (o, List.map su es)
    | E.ArrSelect (a, i)   -> E.ArrSelect (su a, su i)
    | E.ArrStore (a, i, e) -> E.ArrStore (su a, su i, su e)
    | E.FieldSelect (v, e) -> E.FieldSelect (lookup table v, su e)
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
    E.Bi_op (E.Eq, E.Var (lookup table v), rhs) in

  let convert_assignments (e, vs) =
    let e' = substitute e in
    E.mk_and (e' :: List.map assign_to_expr vs) in

  let edge' = match edge with
    | PG.Assert (e, at) -> PG.Assert (substitute e, at)
    | PG.Body b         -> PG.Body (convert_assignments b)
  in

  let vs2' = List.map (lookup table) vs2 in

  ((lbl1, vs1'), (lbl2, vs2'), edge')

let translate g : (PG.lbl * E.var list, ((E.t, E.t) PG.path)) G.t
  = G.map_conns translate_conn g
