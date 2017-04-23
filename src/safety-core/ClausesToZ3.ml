module A  = Z3.Arithmetic
module I  = A.Integer
module R  = A.Real
module FD = Z3.FuncDecl
module B  = Z3.Boolean
module BV = Z3.BitVector
module Ar = Z3.Z3Array
module FP = Z3.Fixedpoint
module P  = Z3.Params
module S  = Z3.Symbol

module T  = Type
module E  = Expr

let ctx = Z3.mk_context ["auto_config", "true"]

let int_sort  = I.mk_sort ctx
let bool_sort = B.mk_sort ctx
let real_sort = R.mk_sort ctx

let rec typ s =
  match s with
  | T.Unit    -> int_sort
  | T.Bool    -> bool_sort
  | T.Int     -> int_sort
  | T.Real    -> real_sort
  | T.Array s -> Ar.mk_sort ctx int_sort (typ s)

type z3_state =
  { context : Z3.context
  ; exprs   : Z3.Expr.expr list
  ; vars    : FD.func_decl list
  ; queries : (Lbl.t * (FD.func_decl * E.query)) list
  }

(** Convert operator to Z3. *)
let op ctx o es =
  let promote2 f = match es with
    | [x;y] -> f ctx x y
    | _     -> assert false in

  (** Z3 supports bitwise operator (such as `left shift`, `and`, etc.), but only
      over special bit vectors. Therefore, to perform such an operation, we
      convert our type to a bit vector, perform the operation, then convert back. *)
  let bitwise f =
    let to_bv     = I.mk_int2bv ctx 64 in
    let from_bv x = BV.mk_bv2int ctx x true in
    promote2 (
      fun ctx x y -> from_bv
          (f ctx (to_bv x) (to_bv y))) in

  match o with
  | E.Unop o -> (match o with
    | E.Not -> B.mk_not ctx (List.hd es))
  | E.Biop o -> (match o with
    | E.IGe  | E.RGe  -> promote2 A.mk_ge
    | E.IGt  | E.RGt  -> promote2 A.mk_gt
    | E.ILe  | E.RLe  -> promote2 A.mk_le
    | E.ILt  | E.RLt  -> promote2 A.mk_lt
    | E.IAdd | E.RAdd -> A.mk_add ctx es
    | E.ISub | E.RSub -> A.mk_sub ctx es
    | E.IMul | E.RMul -> A.mk_mul ctx es
    | E.IDiv | E.RDiv -> promote2 A.mk_div
    | E.IRem | E.RRem -> promote2 I.mk_rem
    | E.Eq            -> promote2 B.mk_eq
    | E.Impl          -> promote2 B.mk_implies
    | E.BAnd          -> bitwise BV.mk_and
    | E.BOr           -> bitwise BV.mk_or
    | E.BXor          -> bitwise BV.mk_xor
    | E.BShl          -> bitwise BV.mk_shl
    | E.BLShr         -> bitwise BV.mk_lshr
    | E.BAShr         -> bitwise BV.mk_ashr)
  | E.Manyop o -> (match o with
    | E.And -> B.mk_and ctx es
    | E.Or  -> B.mk_or  ctx es)

type accessors =
  { vars    : (Var.t * FD.func_decl) list
  ; rels    : (Lbl.t * FD.func_decl) list
  ; queries : (Lbl.t * (FD.func_decl * E.query)) list
  }

(** Convert an expression to Z3. This is done by recursively applying to
    subexpressions. *)
let rec expr special accessors e =
  let ex = expr special accessors in
  let var v = FD.apply (List.assoc v accessors.vars) [] in
  let base = function
    | E.Query (lbl, q, e) ->
      FD.apply (fst (List.assoc lbl accessors.queries)) [ex e]

    | E.Relation ((lbl, _), es) ->
      FD.apply (List.assoc lbl accessors.rels) (List.map ex es)

    | E.Var v           -> var v
    | E.Apply (o, es)   -> (op ctx o) (List.map ex es)
    | E.Store (v, i, e) -> Ar.mk_store ctx (ex v) (ex i) (ex e)
    | E.Select (v, e)   -> Ar.mk_select ctx (ex v) (ex e)
    | E.Int i           -> I.mk_numeral_i ctx i
    | E.Real f          -> R.mk_numeral_s ctx (string_of_float f)
    | E.Bool true       -> B.mk_true ctx
    | E.Bool false      -> B.mk_false ctx
    | E.Allocate o      -> assert false
  in
  Algorithm.specialize base special e

(** Translate a graph where the edges are Horn Clauses to a graph where the edges
    are Z3 expressions. *)
let translate exprs =
  let func_decl n ps t = FD.mk_func_decl ctx (S.mk_string ctx n) ps t in
  let var v = (v, func_decl (Var.as_path v) [] (typ (Var.type_of v))) in
  let relation (lbl, ts) = (lbl, func_decl (Lbl.name lbl) (List.map typ ts) bool_sort) in
  let query (lbl, qinfo) = let (lbl, q) = relation (lbl, [T.Bool]) in (lbl, (q, qinfo)) in

  let upgrade group f = E.exprs_access group exprs |> List.map f in

  let accessors =
    { vars    = upgrade E.vars var
    ; rels    = upgrade E.rels relation
    ; queries = upgrade E.queries query
    } in

  let exprs' = List.map (expr Algorithm.no_special accessors) exprs in
  { context = ctx
  ; exprs   = exprs'
  ; vars    = List.map snd accessors.vars
  ; queries = accessors.queries
  }

let query fp (lbl, (q, E.QueryInfo (at, fname, line))) =
  let show_assert_type = function
    | Assert.Div0        -> "Division by 0 possible"
    | Assert.Null        -> "Null pointer dereference possible"
    | Assert.NegArray    -> "Array access (negative bounds) possible"
    | Assert.ArrayBound  -> "Array access (beyond bounds) possible"
    | Assert.Equivalence -> "Equivalence"
    | Assert.User        -> "User specified property unsafe" in

  match FP.query_r fp [q] with
  | Z3.Solver.SATISFIABLE   ->
    Printf.eprintf "%s at %s line %d\n" (show_assert_type at) fname line;
    exit (Assert.error_code at)

  | Z3.Solver.UNSATISFIABLE -> ();

  | _ ->
    Printf.eprintf "unknown status\n";
    exit 1

let run es =
  let z3_state = translate es in
  let c = z3_state.context in

  let fp = FP.mk_fixedpoint c in
  let r  = P.mk_params c in
  P.add_int r (S.mk_string c "fixedpoint.timeout") 500000;
  P.add_symbol r (S.mk_string c "fixedpoint.engine") (S.mk_string c "duality");
  FP.set_parameters fp r;

  List.iter (fun v -> FP.register_variable fp v) z3_state.vars;
  List.iter (fun clause -> FP.add_rule fp clause None) z3_state.exprs;
  List.iter (query fp) z3_state.queries;

  Printf.eprintf "safe!\n";
  exit 0
