module G = Graph
module PG = Program_graph
module Set = Core.Std.Set.Poly

type lbl = PG.lbl

type sort =
  | Int
  | Bool
  | Real
  | Array of sort

type var = string * sort

type un_op = Not
type bi_op =
  | Eq | Ge | Gt | Le | Lt | Impl
  | Add | Div | Mul | Sub | Rem
  | BAnd | BOr | BXor | BShl | BLShr | BAShr
type many_op = And

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
and query = lbl * expr * PG.assert_type
and rel = lbl * expr list

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
and bi_op_sort op e1 e2 = match op with
  | BAnd | BOr | BXor | BShl | BLShr | BAShr -> Int
  | Eq | Ge | Gt | Le | Lt | Impl -> Bool
  | Add | Sub | Div | Mul | Rem -> expr_sort e1
and many_op_sort = function
  | And -> Bool

let mk_sub e1 e2 = Bi_op (Sub, e1, e2)
let mk_neg e = match expr_sort e with
  | Int -> mk_sub (Int_lit 0) e
  | Real -> mk_sub (Real_lit 0.0) e
  | _ -> Int_lit 0
let mk_not = function
  | Un_op (Not, e) -> e
  | e -> Un_op (Not, e)
let mk_impl e1 e2 = Bi_op (Impl, e1, e2)
let mk_eq e1 e2 = Bi_op (Eq, e1, e2)
let mk_lt e1 e2 = Bi_op (Lt, e1, e2)
let mk_ge e1 e2 = Bi_op (Ge, e1, e2)
let mk_add e1 e2 = Bi_op (Add, e1, e2)
let mk_div e1 e2 = Bi_op (Div, e1, e2)
let mk_mul e1 e2 = Bi_op (Mul, e1, e2)
let mk_rem e1 e2 = Bi_op (Rem, e1, e2)
let mk_bshl e1 e2 = Bi_op (BShl, e1, e2)
let mk_blshr e1 e2 = Bi_op (BLShr, e1, e2)
let mk_bashr e1 e2 = Bi_op (BAShr, e1, e2)
let mk_band e1 e2 = Bi_op (BAnd, e1, e2)
let mk_bor  e1 e2 = Bi_op (BOr,  e1, e2)
let mk_bxor e1 e2 = Bi_op (BXor, e1, e2)
let mk_and es =
  let es' = List.filter ((<>) True) es in
  match es' with
  | [] -> True
  | [e] -> e
  | es -> Many_op (And, es)

let mk_assign v e = (v, e)

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
    | Un_op (_, e)       -> ex e
    | Bi_op (_, e1, e2)  -> f (ex e1) (ex e2)
    | Many_op (_, es)    -> List.fold_left f zero (List.map ex es)
    | ArrSelect (a, i)   -> f (ex a) (ex i)
    | ArrStore (a, i, e) -> List.fold_left f zero [ex a; ex i; ex e]
    | _                  -> zero

(** Find the variables in an expression *)
let expr_vars =
  let special_case = function
    | Var v            -> Some (Set.singleton v)
    | _                -> None
  in fold_expr special_case Set.union Set.empty

let path_uses_before_assigns p =
  let loop (a_set, u_set) (v, e) =
    (* Add the variables in e to the use set if they were not in the assignment set. *)
    let augment e =
      Set.union
        (Set.diff (expr_vars e) a_set)
        u_set in

    (Set.add a_set v, augment e)
  in

  match p with
  | PG.Relate -> Set.empty
  | PG.Assert (e, _) -> expr_vars e
  | PG.Body (e, assigns) ->
    Set.union
      (expr_vars e)
      (snd (List.fold_left loop (Set.empty, Set.empty) assigns))

let path_assigns = function
  | PG.Relate | PG.Assert _ -> Set.empty
  | PG.Body (e, assigns) -> List.map fst assigns |> Set.of_list

let nested_map f xs =
  List.map (List.map f) xs

let critical_vars p lbl =
  let coming = G.walks_to p lbl |> nested_map G.edge in
  let going = G.walks_from p lbl |> nested_map G.edge in

  if List.length going = 0 then
    (nested_map path_assigns coming |> List.concat |> Set.union_list)
  else
    Set.inter
      (nested_map path_assigns coming |> List.concat |> Set.union_list)
      (nested_map path_uses_before_assigns going |> List.concat|> Set.union_list)
