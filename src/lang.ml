type sort =
  | Int
  | Bool
  | Real
  | Array of sort

type var = string * sort

module V_set = Set_ext.Make(
  struct type t = var;; let compare = compare end)

type lbl = int

type un_op = Not | Neg
type bi_op = Eq | Ge | Gt | Le | Lt | Impl | Add | Div | Mul | Rem
type many_op = And

type assert_type =
  | Div0
  | Null
  | ArrayBound
  | User

let show_assert_type = function
  | Div0       -> "Division by 0?"
  | Null       -> "Null?"
  | ArrayBound -> "Array out of bound?"
  | User       -> "User specified:"

type expr =
  | Relation of rel
  | Query of query
  | Var of var
  | Un_op of un_op * expr
  | Bi_op of bi_op * expr * expr
  | Many_op of many_op * expr list
  | ArrStore of expr * expr * expr
  | ArrSelect of expr * expr
  | Int_lit of int
  | Real_lit of float
  | True
  | False
  | Any of sort
and query = lbl * expr * assert_type
and rel = lbl * expr list

type instr =
  | Relate of lbl
  | Assert of expr * assert_type
  | Assign of var * expr
  | Call of expr

let rec inner_sort = function
  | Array s -> s
  | s -> s

let rec expr_sort = function
  | Relation r         -> Bool
  | Query q            -> Bool
  | Var (name, s)      -> s
  | Un_op (op, e)      -> un_op_sort op e
  | Bi_op (op, e1, e2) -> bi_op_sort op e1 e2
  | Many_op (op, _)    -> many_op_sort op
  | ArrStore (arr,_,e) -> expr_sort arr
  | ArrSelect (arr,_)  -> inner_sort (expr_sort arr)
  | Int_lit _          -> Int
  | Real_lit _         -> Real
  | True               -> Bool
  | False              -> Bool
  | Any s              -> s

and un_op_sort op e = match op with
  | Not -> Bool
  | Neg -> expr_sort e
and bi_op_sort op e1 e2 = match op with
  | Eq | Ge | Gt | Le | Lt | Impl -> Bool
  | Add | Div | Mul | Rem -> expr_sort e1
and many_op_sort = function
  | And -> Bool

let mk_neg e = Un_op (Neg, e)
let mk_not = function
  | Un_op (Not, e) -> e
  | e -> Un_op (Not, e)
let mk_impl e1 e2 = Bi_op (Impl, e1, e2)
let mk_eq e1 e2 = Bi_op (Eq, e1, e2)
let mk_lt e1 e2 = Bi_op (Lt, e1, e2)
let mk_add e1 e2 = Bi_op (Add, e1, e2)
let mk_div e1 e2 = Bi_op (Div, e1, e2)
let mk_mul e1 e2 = Bi_op (Mul, e1, e2)
let mk_rem e1 e2 = Bi_op (Rem, e1, e2)
let mk_and es =
  let es' = List.filter ((<>) True) es in
  match es' with
  | [e] -> e
  | es -> Many_op (And, es)

let mk_assign v e = Assign (v, e)

module R_set = Set_ext.Make(
  struct type t = rel;; let compare (l1, _) (l2, _) = compare l1 l2 end)

module Q_set = Set_ext.Make(
  struct type t = query;; let compare = compare end)

module PG = Graph.Make(
  struct type node = lbl;; type edge = instr list end)

(** A common pattern is to recurse over subexpressions to calculate some value.
    `fold_expr` encapsulates this recursion. The user provides a `special_case`
    function which indicates when an expression yields a result. When
    `special_case` returns `None`, fold_expr recursively applies to the
    subexpressions and combines the results using `f`. If there are no
    subexpressions, then returns `zero`. *)
let rec fold_expr special_case f zero e =
  let ex = fold_expr special_case f zero in
  match special_case e with
  | Some r -> r
  | None -> match e with
    | Relation (_, es)   -> List.fold_left f zero (List.map ex es)
    | Query (_, e, _)    -> ex e
    | Un_op (_, e)       -> ex e
    | Bi_op (_, e1, e2)  -> f (ex e1) (ex e2)
    | Many_op (_, es)    -> List.fold_left f zero (List.map ex es)
    | ArrSelect (a, i)   -> f (ex a) (ex i)
    | ArrStore (a, i, e) -> List.fold_left f zero [ex a; ex i; ex e]
    | _                  -> zero

(** Find the queries in an expression *)
let queries =
  let special_case = function
    | Query q -> Some (Q_set.singleton q)
    | _       -> None
  in fold_expr special_case Q_set.union Q_set.empty

(** Find the relations in an expression *)
let expr_rels =
  let special_case = function
    | Relation r -> Some (R_set.singleton r)
    | _ -> None
  in fold_expr special_case R_set.union R_set.empty

(** Find the variables in an expression *)
let expr_vars =
  let special_case = function
    | Var v            -> Some (V_set.singleton v)
    | _                -> None
  in fold_expr special_case V_set.union V_set.empty

let path_uses_before_assigns is =
  let loop (a_set, u_set) i =
    (* Add all the variables in the expression to the use set that were not in
       the assignment set. *)
    let augment e =
      V_set.union
        (V_set.diff (expr_vars e) a_set)
        u_set in

    match i with
    | Assign (v, e) -> (V_set.add v a_set, augment e)
    | Assert (e, _) -> (a_set, augment e)
    | Call e        -> (a_set, augment e)
    | Relate _      -> (a_set, u_set)
  in
  snd (List.fold_left loop (V_set.empty, V_set.empty) is)

let path_assigns =
  V_set.unions_map (function
      | Assign (v, _) -> V_set.singleton v
      | _ -> V_set.empty)


let connect_path path =
  path
  |> List.map PG.edge
  |> List.fold_left (@) []

let critical_vars p lbl =
  let coming = PG.paths_to p lbl |> List.map connect_path in
  let going = PG.paths_from p lbl |> List.map connect_path in

  V_set.add ("ID", Int)
    (if List.length going = 0 then
       (List.map path_assigns coming |> V_set.unions)
     else
       V_set.inter
         (List.map path_assigns coming |> V_set.unions)
         (List.map path_uses_before_assigns going |> V_set.unions))
