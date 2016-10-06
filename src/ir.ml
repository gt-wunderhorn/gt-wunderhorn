type label = int

type sort =
  | Array of sort
  | Int
  | Bool

type variable = Variable of string * sort

type procedure_id = Javalib_pack.JBasics.class_method_signature

type expression =
  | Relation  of string * variable list
  | Query     of variable
  | Var       of variable
  | ArrStore  of expression * expression * expression
  | ArrSelect of expression * expression
  | Add       of expression * expression
  | Eq        of expression * expression
  | Ge        of expression * expression
  | Gt        of expression * expression
  | Le        of expression * expression
  | Lt        of expression * expression
  | Implies   of expression * expression
  | And       of expression list
  | Not       of expression
  | Int_lit   of int
  | True
  | False

type linear_instruction =
  | Assign  of variable * expression
  | Call    of expression
  | Assert  of variable

type non_linear_instruction =
  | If      of expression * label
  | Goto    of label
  | Invoke  of variable * procedure_id * expression list
  | Return  of expression

type instruction =
  | Linear     of linear_instruction
  | Non_linear of non_linear_instruction

module Var_set = Set_ext.Make(
  struct type t = variable;; let compare = compare;; end)

module Rel_set = Set_ext.Make(
  struct
    type t = string * variable list
    let compare (lbl1, _) (lbl2, _) = compare lbl1 lbl2
  end)

let rec expr_sort = function
  | ArrStore _ -> Int (* TODO *)
  | ArrSelect (arr, _) -> (match expr_sort arr with
    | Array s -> s
    | _       -> assert false (* array select from non-array *))
  | Relation _            -> Bool
  | Query _               -> Bool
  | Var (Variable (_, s)) -> s
  | Add _                 -> Int
  | Eq _                  -> Bool
  | Ge _                  -> Bool
  | Gt _                  -> Bool
  | Le _                  -> Bool
  | Lt _                  -> Bool
  | Implies _             -> Bool
  | And _                 -> Bool
  | Not _                 -> Bool
  | Int_lit _             -> Int
  | True                  -> Bool
  | False                 -> Bool

let rec expr_vars = function
  | Relation (_, vs)     -> Var_set.unions_map Var_set.singleton vs
  | Query v              -> Var_set.singleton v
  | Var v                -> Var_set.singleton v
  | ArrStore (e1, e2, e3) -> Var_set.unions [ expr_vars e1; expr_vars e2; expr_vars e3]
  | ArrSelect (e1, e2)   -> Var_set.union (expr_vars e1) (expr_vars e2)
  | Add (e1, e2)         -> Var_set.union (expr_vars e1) (expr_vars e2)
  | Eq (e1, e2)          -> Var_set.union (expr_vars e1) (expr_vars e2)
  | Ge (e1, e2)          -> Var_set.union (expr_vars e1) (expr_vars e2)
  | Gt (e1, e2)          -> Var_set.union (expr_vars e1) (expr_vars e2)
  | Le (e1, e2)          -> Var_set.union (expr_vars e1) (expr_vars e2)
  | Lt (e1, e2)          -> Var_set.union (expr_vars e1) (expr_vars e2)
  | Implies (e1, e2)     -> Var_set.union (expr_vars e1) (expr_vars e2)
  | And es               -> Var_set.unions_map expr_vars es
  | Not e                -> expr_vars e
  | Int_lit _            -> Var_set.empty
  | True                 -> Var_set.empty
  | False                -> Var_set.empty

let rec expr_rels = function
  | Relation (l, vs)     -> Rel_set.singleton (l, vs)
  | Query v              -> Rel_set.empty
  | Var v                -> Rel_set.empty
  | ArrStore (v, e1, e2) -> Rel_set.union (expr_rels e1) (expr_rels e2)
  | ArrSelect (e1, e2)   -> Rel_set.union (expr_rels e1) (expr_rels e2)
  | Add (e1, e2)         -> Rel_set.union (expr_rels e1) (expr_rels e2)
  | Eq (e1, e2)          -> Rel_set.union (expr_rels e1) (expr_rels e2)
  | Ge (e1, e2)          -> Rel_set.union (expr_rels e1) (expr_rels e2)
  | Gt (e1, e2)          -> Rel_set.union (expr_rels e1) (expr_rels e2)
  | Le (e1, e2)          -> Rel_set.union (expr_rels e1) (expr_rels e2)
  | Lt (e1, e2)          -> Rel_set.union (expr_rels e1) (expr_rels e2)
  | Implies (e1, e2)     -> Rel_set.union (expr_rels e1) (expr_rels e2)
  | And es               -> Rel_set.unions_map expr_rels es
  | Not e                -> expr_rels e
  | Int_lit _            -> Rel_set.empty
  | True                 -> Rel_set.empty
  | False                -> Rel_set.empty

(** Which variables are used by a given instruction? *)
let instruction_useds = function
  | Assign (v, e)     -> expr_vars e
  | Call e            -> expr_vars e
  | Assert v          -> Var_set.singleton v

(** Which variables are mutated by a given instruction? *)
let instruction_mutables = function
  | Assign (v, _)    -> Var_set.singleton v
  | _                -> Var_set.empty

let instruction_variables i =
  Var_set.union (instruction_useds i) (instruction_mutables i)

let mk_not = function
  | Not e -> e
  | e     -> Not e

let mk_and = function
  | [e] -> e
  | es  -> And es

let rec num_queries es =
  let rec count = function
    | Query _          -> 1
    | Eq (e1, e2)      -> count e1 + count e2
    | Implies (e1, e2) -> count e1 + count e2
    | And es           -> num_queries es
    | Not e            -> count e
    | _                -> 0 in
  List.map count es |> List.fold_left (+) 0
