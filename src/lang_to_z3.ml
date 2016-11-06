module S = Z3.Symbol
module FD = Z3.FuncDecl
module A = Z3.Arithmetic
module I = A.Integer
module R = A.Real
module B = Z3.Boolean
module BV = Z3.BitVector
module Ar = Z3.Z3Array

module L = Lang
module PG = Program_graph
module G = Graph
module Set = Core.Std.Set.Poly

let c  = Z3.mk_context []
let int_sort = I.mk_sort c
let int_array_sort = Ar.mk_sort c int_sort int_sort
let bool_sort = B.mk_sort c
let bool_array_sort = Ar.mk_sort c int_sort bool_sort
let real_sort = R.mk_sort c
let real_array_sort = Ar.mk_sort c int_sort real_sort

let sorts  = [ L.Bool, B.mk_sort c
             ; L.Int,  int_sort
             ; L.Real, real_sort
             ; L.Array L.Int, int_array_sort
             ; L.Array (L.Array L.Int), Ar.mk_sort c int_sort int_array_sort
             ; L.Array L.Bool, bool_array_sort
             ; L.Array (L.Array L.Bool), Ar.mk_sort c int_sort bool_array_sort
             ; L.Array L.Real, real_array_sort
             ; L.Array (L.Array L.Real), Ar.mk_sort c int_sort real_array_sort
             ]

let find_sort s = List.assoc s sorts

type z3_state =
  { context : Z3.context
  ; graph   : (L.lbl, Z3.Expr.expr) G.t
  ; vars    : FD.func_decl list
  ; queries : (FD.func_decl * PG.assert_type) list
  }

let var (name, sort) =
  let v = FD.mk_func_decl c (S.mk_string c name) [] (find_sort sort) in
  ((name, sort), v)

let relation (name, es) =
  let sorts = List.map (fun e -> L.expr_sort e) es in
  let sorts = List.map find_sort sorts in
  let rel = FD.mk_func_decl c (S.mk_string c name) sorts (B.mk_sort c) in
  (name, rel)

let rec expr rels vars e =
  let ex = expr rels vars in

  let un_op = function
    | L.Not -> B.mk_not
  in

  let bi_op o =
    let bitwise f c x y =
      BV.mk_bv2int c (f c (I.mk_int2bv c 64 x) (I.mk_int2bv c 64 y)) true in

    match o with
    | L.Eq    -> B.mk_eq
    | L.Ge    -> A.mk_ge
    | L.Gt    -> A.mk_gt
    | L.Le    -> A.mk_le
    | L.Lt    -> A.mk_lt
    | L.Impl  -> B.mk_implies
    | L.Add   -> fun c x y -> A.mk_add c [x; y]
    | L.Sub   -> fun c x y -> A.mk_sub c [x; y]
    | L.Div   -> A.mk_div
    | L.Mul   -> fun c x y -> A.mk_mul c [x; y]
    | L.Rem   -> I.mk_rem
    | L.BAnd  -> bitwise BV.mk_and
    | L.BOr   -> bitwise BV.mk_or
    | L.BXor  -> bitwise BV.mk_xor
    | L.BShl  -> bitwise BV.mk_shl
    | L.BLShr -> bitwise BV.mk_lshr
    | L.BAShr -> bitwise BV.mk_ashr
  in

  let many_op = function
    | L.And -> B.mk_and
  in

  match e with
  | L.Query (lbl, e, at) ->
    let name = "q_" ^ string_of_int lbl in
    FD.apply (List.assoc name rels) [ex e]
  | L.Relation (lbl, es) ->
    let name = "r_" ^ string_of_int lbl in
    FD.apply (List.assoc name rels) (List.map ex es)

  | L.Var v                  -> FD.apply (List.assoc v vars) []
  | L.Un_op (o, e)           -> (un_op o c) (ex e)
  | L.Bi_op (o, x, y)        -> (bi_op o c) (ex x) (ex y)
  | L.Many_op (o, es)        -> (many_op o c) (List.map ex es)
  | L.ArrStore (arr, idx, e) -> Ar.mk_store c (ex arr) (ex idx) (ex e)
  | L.ArrSelect (e1, e2)     -> Ar.mk_select c (ex e1) (ex e2)
  | L.Int_lit i              -> I.mk_numeral_i c i
  | L.Real_lit f             -> R.mk_numeral_s c (string_of_float f)
  | L.True                   -> B.mk_true c
  | L.False                  -> B.mk_false c
  | L.Any s                  -> assert false

let query rels (lbl, e, at) =
  let name = "q_" ^ string_of_int lbl in
  (List.assoc name rels, at)

let union_map_list f xs = Set.union_list (List.map f xs)
let r_union_map_list f xs = List.fold_left L.R_set.union L.R_set.empty (List.map f xs)

let translate graph =
  let exprs = G.edges graph in

  let vars = union_map_list L.expr_vars exprs |> Set.to_list in
  let rels = r_union_map_list L.expr_rels exprs |> L.R_set.elements in
  let queries = union_map_list L.queries exprs |> Set.to_list in

  let vars' = List.map var vars in
  let rels' =
    List.map (fun (lbl, es) -> relation ("r_" ^ string_of_int lbl, es)) rels
    @
    List.map (fun (lbl, e, at) -> relation ("q_" ^ string_of_int lbl, [e])) queries in

  let queries' = List.map (query rels') queries in

  let g = G.map_edges (expr rels' vars') graph in
  { context = c
  ; graph   = g
  ; vars    = List.map snd vars'
  ; queries = queries'
  }
