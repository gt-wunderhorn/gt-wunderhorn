module P = Z3.Params
module S = Z3.Symbol
module FD = Z3.FuncDecl
module FP = Z3.Fixedpoint
module A = Z3.Arithmetic
module I = A.Integer
module R = A.Real
module B = Z3.Boolean
module BV = Z3.BitVector
module F = Z3.FloatingPoint
module Ar = Z3.Z3Array

module L = Lang
module PG = Program_graph
module Set = Core.Std.Set.Poly

type context = { c  : Z3.context
               ; fp : FP.fixedpoint
               ; mutable vars : (L.var * Z3.Expr.expr) list
               ; mutable rels : (string * FD.func_decl) list
               ; sorts : (L.sort * Z3.Sort.sort) list
               }

let find_sort c s = List.assoc s c.sorts

let interpret_var c (name, sort) =
  let v' = FD.mk_func_decl c.c (S.mk_string c.c name) [] (find_sort c sort) in
  let v = FD.apply v' [] in
  FP.register_variable c.fp v';
  c.vars <- c.vars @ [((name, sort), v)]

let interpret_relation c (name, es) =
  let sorts = List.map (fun e -> L.expr_sort e) es in
  let sorts = List.map (find_sort c) sorts in
  let rel = FD.mk_func_decl c.c (S.mk_string c.c name) sorts (B.mk_sort c.c) in
  c.rels <- (name, rel) :: c.rels

let rec interpret_expr c e =
  let ex = interpret_expr c in

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
    FD.apply (List.assoc name c.rels) [ex e]
  | L.Relation (lbl, es) ->
    let name = "r_" ^ string_of_int lbl in
    FD.apply (List.assoc name c.rels) (List.map ex es)

  | L.Var v                  -> List.assoc v c.vars
  | L.Un_op (o, e)           -> (un_op o c.c) (ex e)
  | L.Bi_op (o, x, y)        -> (bi_op o c.c) (ex x) (ex y)
  | L.Many_op (o, es)        -> (many_op o c.c) (List.map ex es)
  | L.ArrStore (arr, idx, e) -> Ar.mk_store c.c (ex arr) (ex idx) (ex e)
  | L.ArrSelect (e1, e2)     -> Ar.mk_select c.c (ex e1) (ex e2)
  | L.Int_lit i  -> I.mk_numeral_i c.c i
  | L.Real_lit f -> R.mk_numeral_s c.c (string_of_float f)
  | L.True       -> B.mk_true c.c
  | L.False      -> B.mk_false c.c
  | L.Any s      -> assert false

let interpret_clause c clause =
  FP.add_rule c.fp (interpret_expr c clause) None

let interpret_query c (lbl, e, at) =
  let name = "q_" ^ string_of_int lbl in
  let f = List.assoc name c.rels  in

  let res = FP.query_r c.fp [f] in
  match res with
  | Z3.Solver.SATISFIABLE   -> Printf.printf "%s unsafe\n" (PG.show_assert_type at)
  | Z3.Solver.UNSATISFIABLE -> Printf.printf "%s safe\n" (PG.show_assert_type at)
  | _ -> Printf.printf "unknown\n"

let initialize () =
  let c  = Z3.mk_context [] in
  let fp = FP.mk_fixedpoint c in
  let r  = P.mk_params c in
  P.add_int r (S.mk_string c "fixedpoint.timeout") 120000;
  P.add_symbol r (S.mk_string c "fixedpoint.engine") (S.mk_string c "duality");
  FP.set_parameters fp r;

  let int_sort = I.mk_sort c in
  let int_array_sort = Ar.mk_sort c int_sort int_sort in
  let bool_sort = B.mk_sort c in
  let bool_array_sort = Ar.mk_sort c int_sort bool_sort in
  let real_sort = R.mk_sort c in
  let real_array_sort = Ar.mk_sort c int_sort real_sort in

  { c      = c
  ; fp     = fp
  ; vars   = []
  ; rels   = []
  ; sorts  = [ (L.Bool, B.mk_sort c)
             ; (L.Int,  int_sort)
             ; (L.Real, real_sort)
             ; (L.Array L.Int, int_array_sort)
             ; (L.Array (L.Array L.Int), Ar.mk_sort c int_sort int_array_sort)
             ; (L.Array L.Bool, bool_array_sort)
             ; (L.Array (L.Array L.Bool), Ar.mk_sort c int_sort bool_array_sort)
             ; (L.Array L.Real, real_array_sort)
             ; (L.Array (L.Array L.Real), Ar.mk_sort c int_sort real_array_sort)
             ]
  }

let union_map_list f xs = Set.union_list (List.map f xs)
let r_union_map_list f xs = List.fold_left L.R_set.union L.R_set.empty (List.map f xs)

let run exprs =
  let vars = union_map_list L.expr_vars exprs in
  let rels = r_union_map_list L.expr_rels exprs in
  let queries = union_map_list L.queries exprs in

  let c = initialize () in

  Set.iter ~f:(interpret_var c) vars;
  L.R_set.iter (fun (lbl, es) ->
      interpret_relation c ("r_" ^ string_of_int lbl, es)) rels;
  Set.iter ~f:(fun (lbl, e, at) ->
      interpret_relation c ("q_" ^ string_of_int lbl, [e])) queries;
  List.iter (interpret_clause c) exprs;
  Set.iter ~f:(interpret_query c) queries
