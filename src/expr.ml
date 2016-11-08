module G = Graph
module PG = Program_graph
module S = Core.Std.Set.Poly

type sort = Int | Bool | Real | Array of sort

type var = string * sort

type un_op = Not
type bi_op =
  | Eq | Ge | Gt | Le | Lt | Impl
  | Add | Div | Mul | Sub | Rem
  | BAnd | BOr | BXor | BShl | BLShr | BAShr
type many_op = And

type t =
  | Relation of rel
  | Query of query
  | Var of var
  | Un_op of un_op * t
  | Bi_op of bi_op * t * t
  | Many_op of many_op * t list
  | ArrStore of t * t * t
  | ArrSelect of t * t
  | FieldSelect of var * t
  | Int_lit of int
  | Real_lit of float
  | True
  | False
  | Any of sort
and query = PG.lbl * t * PG.assert_type
and rel = PG.lbl * t list

let rec inner_sort = function
  | Array s -> s
  | s -> s

let rec sort_of = function
  | Relation r             -> Bool
  | Query q                -> Bool
  | Var (name, s)          -> s
  | Un_op (op, e)          -> un_op_sort op e
  | Bi_op (op, e1, e2)     -> bi_op_sort op e1 e2
  | Many_op (op, _)        -> many_op_sort op
  | ArrStore (arr,_,e)     -> sort_of arr
  | ArrSelect (arr,_)      -> inner_sort (sort_of arr)
  | FieldSelect ((v, s),_) -> s
  | Int_lit _              -> Int
  | Real_lit _             -> Real
  | True                   -> Bool
  | False                  -> Bool
  | Any s                  -> s

and un_op_sort op e = match op with
  | Not -> Bool
and bi_op_sort op e1 e2 = match op with
  | BAnd | BOr | BXor | BShl | BLShr | BAShr -> Int
  | Eq | Ge | Gt | Le | Lt | Impl -> Bool
  | Add | Sub | Div | Mul | Rem -> sort_of e1
and many_op_sort = function
  | And -> Bool

let mk_not = function
  | Un_op (Not, e) -> e
  | e -> Un_op (Not, e)

let simplifying_biop fi fr op e1 e2 = match (e1, e2) with
  | (Int_lit x, Int_lit y)   -> Int_lit (fi x y)
  | (Real_lit x, Real_lit y) -> Real_lit (fr x y)
  | _ -> Bi_op (op, e1, e2)

let mk_add = simplifying_biop ( + ) ( +. ) Add
let mk_mul = simplifying_biop ( * ) ( *. ) Mul
let mk_sub = simplifying_biop ( - ) ( -. ) Sub
let mk_div = simplifying_biop ( / ) ( /. ) Div

let mk_neg e = match sort_of e with
  | Int -> mk_sub (Int_lit 0) e
  | Real -> mk_sub (Real_lit 0.0) e
  | _ -> Int_lit 0

let mk_biop op e1 e2 = Bi_op (op, e1, e2)

let mk_impl  = mk_biop Impl
let mk_eq    = mk_biop Eq
let mk_lt    = mk_biop Lt
let mk_ge    = mk_biop Ge
let mk_rem   = mk_biop Rem
let mk_bshl  = mk_biop BShl
let mk_blshr = mk_biop BLShr
let mk_bashr = mk_biop BAShr
let mk_band  = mk_biop BAnd
let mk_bor   = mk_biop BOr
let mk_bxor  = mk_biop BXor
let mk_and es =
  let es' = List.filter ((<>) True) es in
  match es' with
  | [] -> True
  | [e] -> e
  | es -> Many_op (And, es)

let mk_assign v e = (v, e)

(** A common pattern is to recurse over subexpressions to calculate some value.
    `fold` encapsulates this recursion. The user provides a `special_case`
    function which indicates when an expression yields a result. When
    `special_case` returns `None`, fold recursively applies to the
    subexpressions and combines the results using `f`. If there are no
    subexpressions, then returns `zero`. *)
let rec fold special f zero e =
  let ex = fold special f zero in
  let base = function
    | Relation (_, es)   -> List.fold_left f zero (List.map ex es)
    | Query (_, e, _)    -> ex e
    | Un_op (_, e)       -> ex e
    | Bi_op (_, e1, e2)  -> f (ex e1) (ex e2)
    | Many_op (_, es)    -> List.fold_left f zero (List.map ex es)
    | ArrSelect (a, i)   -> f (ex a) (ex i)
    | ArrStore (a, i, e) -> List.fold_left f zero [ex a; ex i; ex e]
    | FieldSelect (_, e) -> ex e
    | _                  -> zero
  in
  Special.specialize base special e

(** Find the variables in an expression *)
let rec vars e =
  let special_case = function
    | FieldSelect (v, e) -> Some (S.union (S.singleton v) (vars e))
    | Var v -> Some (S.singleton v)
    | _     -> None
  in fold special_case S.union S.empty e

(** Find the queries in an expression *)
let queries =
  let special_case = function
    | Query q -> Some (S.singleton q)
    | _       -> None
  in fold special_case S.union S.empty

module R_set = Set.Make(
  struct
    type t = rel
    let compare (lbl1, _) (lbl2, _) = compare lbl1 lbl2
  end)

(** Find the relations in an expression *)
let rels =
  let special_case = function
    | Relation r -> Some (R_set.singleton r)
    | _ -> None
  in fold special_case R_set.union R_set.empty
