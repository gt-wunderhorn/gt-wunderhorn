module S = Z3.Symbol
module FD = Z3.FuncDecl
module A = Z3.Arithmetic
module I = A.Integer
module R = A.Real
module B = Z3.Boolean
module BV = Z3.BitVector
module Ar = Z3.Z3Array

module E = Expr
module PG = Program_graph
module G = Graph
module Set = Core.Std.Set.Poly

let ctx = Z3.mk_context []

let rec sort s =
  let int_sort = I.mk_sort ctx in
  match s with
  | E.Bool    -> B.mk_sort ctx
  | E.Int     -> int_sort
  | E.Real    -> R.mk_sort ctx
  | E.Array s -> Ar.mk_sort ctx int_sort (sort s)

type z3_state =
  { context : Z3.context
  ; graph   : (PG.lbl, Z3.Expr.expr) G.t
  ; vars    : FD.func_decl list
  ; queries : (FD.func_decl * PG.assert_type) list
  }

let var (name, s) =
  let v = FD.mk_func_decl ctx (S.mk_string ctx name) [] (sort s) in
  ((name, s), v)

let relation (name, es) =
  let ss = List.map (fun e -> E.sort_of e) es in
  let sorts' = List.map sort ss in
  let rel = FD.mk_func_decl ctx (S.mk_string ctx name) sorts' (B.mk_sort ctx) in
  (name, rel)

(** Convert unary operator to Z3. *)
let un_op = function
  | E.Not -> B.mk_not

(** Convert binary operator to Z3. *)
let bi_op o =
  (** Z3 supports bitwise operator (such as `left shift`, `and`, etc.), but only
      over special bit vectors. Therefore, to perform such an operation, we
      convert our type to a bit vector, perform the operation, then convert back. *)
  let bitwise f ctx x y =
    BV.mk_bv2int ctx (f ctx (I.mk_int2bv ctx 64 x) (I.mk_int2bv ctx 64 y)) true in

  match o with
  | E.Eq    -> B.mk_eq
  | E.Ge    -> A.mk_ge
  | E.Gt    -> A.mk_gt
  | E.Le    -> A.mk_le
  | E.Lt    -> A.mk_lt
  | E.Impl  -> B.mk_implies
  | E.Add   -> fun ctx x y -> A.mk_add ctx [x; y]
  | E.Sub   -> fun ctx x y -> A.mk_sub ctx [x; y]
  | E.Div   -> A.mk_div
  | E.Mul   -> fun ctx x y -> A.mk_mul ctx [x; y]
  | E.Rem   -> I.mk_rem
  | E.BAnd  -> bitwise BV.mk_and
  | E.BOr   -> bitwise BV.mk_or
  | E.BXor  -> bitwise BV.mk_xor
  | E.BShl  -> bitwise BV.mk_shl
  | E.BLShr -> bitwise BV.mk_lshr
  | E.BAShr -> bitwise BV.mk_ashr

(** Convert operator over list of operands to Z3. *)
let many_op = function
  | E.And -> B.mk_and

(** Convert an expression to Z3. This is done by recursively applying to
    subexpressions. *)
let rec expr special rels vars e =
  let ex = expr special rels vars in

  let base = function
    | E.Query (lbl, e, at) ->
      let name = "q_" ^ string_of_int lbl in
      FD.apply (List.assoc name rels) [ex e]
    | E.Relation (lbl, es) ->
      let name = "r_" ^ string_of_int lbl in
      FD.apply (List.assoc name rels) (List.map ex es)

    | E.Var v                  -> FD.apply (List.assoc v vars) []
    | E.Un_op (o, e)           -> (un_op o ctx) (ex e)
    | E.Bi_op (o, x, y)        -> (bi_op o ctx) (ex x) (ex y)
    | E.Many_op (o, es)        -> (many_op o ctx) (List.map ex es)
    | E.ArrStore (arr, idx, e) -> Ar.mk_store ctx (ex arr) (ex idx) (ex e)
    | E.ArrSelect (e1, e2)     -> Ar.mk_select ctx (ex e1) (ex e2)
    | E.FieldSelect (v, e)     -> Ar.mk_select ctx (ex (E.Var v)) (ex e)
    | E.Int_lit i              -> I.mk_numeral_i ctx i
    | E.Real_lit f             -> R.mk_numeral_s ctx (string_of_float f)
    | E.True                   -> B.mk_true ctx
    | E.False                  -> B.mk_false ctx
    | E.Any s                  -> assert false in
  Special.specialize base special e

let query rels (lbl, e, at) =
  let name = "q_" ^ string_of_int lbl in
  (List.assoc name rels, at)

let union_map_list f xs = Set.union_list (List.map f xs)
let r_union_map_list f xs = List.fold_left E.R_set.union E.R_set.empty (List.map f xs)

(** Translate a graph where the edges are Horn Clauses to a graph where the edges
    are Z3 expressions. *)
let translate graph =
  let exprs = G.edges graph in

  let vars = union_map_list E.vars exprs |> Set.to_list in
  let rels = r_union_map_list E.rels exprs |> E.R_set.elements in
  let queries = union_map_list E.queries exprs |> Set.to_list in

  let vars' = List.map var vars in
  let rels' =
    List.map (fun (lbl, es) -> relation ("r_" ^ string_of_int lbl, es)) rels
    @
    List.map (fun (lbl, e, at) -> relation ("q_" ^ string_of_int lbl, [e])) queries in

  let queries' = List.map (query rels') queries in

  let g = G.map_edges (expr Special.no_special rels' vars') graph in
  { context = ctx
  ; graph   = g
  ; vars    = List.map snd vars'
  ; queries = queries'
  }
