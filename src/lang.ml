type var = string

module V_set = Set_ext.Make(
  struct type t = var;; let compare = compare end)

type lbl = string

type rel = lbl * var list

type un_op = Not
type bi_op = Eq | Ge | Gt | Le | Lt | Impl | Add
type many_op = And

type expr =
  | Relation of rel
  | Query of query
  | Var of var
  | Un_op of un_op * expr
  | Bi_op of bi_op * expr * expr
  | Many_op of many_op * expr list
  | Int_lit of int
  | True
  | False
and query = lbl * expr

type instr =
  | Relate of lbl
  | Assert of expr
  | Assign of var * expr
  | Call of expr

let mk_not = function
  | Un_op (Not, e) -> e
  | e -> Un_op (Not, e)
let mk_impl e1 e2 = Bi_op (Impl, e1, e2)
let mk_eq e1 e2 = Bi_op (Eq, e1, e2)
let mk_and = function
  | [e] -> e
  | es -> Many_op (And, es)

module R_set = Set_ext.Make(
  struct type t = rel;; let compare (l1, _) (l2, _) = compare l1 l2 end)

module Q_set = Set_ext.Make(
  struct type t = query;; let compare = compare end)

module PG = Graph.Make(
  struct type node = lbl;; type edge = instr list end)

let instr_assigns = function
  | Assign (v, _) -> [v]
  | _ -> []

let expr_vars e =
  let rec ex = function
    | Relation (_, vs)  -> V_set.of_list vs
    | Var v             -> V_set.singleton v
    | Un_op (_, e)      -> ex e
    | Bi_op (_, e1, e2) -> V_set.union (ex e1) (ex e2)
    | Many_op (_, es)   -> V_set.unions_map ex es
    | _                 -> V_set.empty
  in ex e

let expr_rels e =
  let rec ex = function
    | Relation r        -> R_set.singleton r
    | Un_op (_, e)      -> ex e
    | Bi_op (_, e1, e2) -> R_set.union (ex e1) (ex e2)
    | Many_op (_, es)   -> R_set.unions_map ex es
    | _                 -> R_set.empty
  in ex e

let queries e =
  let rec ex = function
    | Query q           -> Q_set.singleton (q)
    | Un_op (_, e)      -> ex e
    | Bi_op (_, e1, e2) -> Q_set.union (ex e1) (ex e2)
    | Many_op (_, es)   -> Q_set.unions_map ex es
    | _                 -> Q_set.empty
  in ex e

let instr_uses instr =
  let ex e = expr_vars e |> V_set.elements in

  match instr with
  | Relate _ -> []
  | Assert e -> ex e
  | Assign (_, e) -> ex e
  | Call e -> ex e

let path_var_locations var_finder path =
  path
  |> List.mapi (fun line instr -> (line, var_finder instr))
  |> List.fold_left (fun dict (line, vs) ->
      List.fold_left (fun dict v ->
          if List.mem_assoc v dict
          then dict
          else (v, line) :: dict) dict vs) []

let path_assign_locations = path_var_locations instr_assigns
let path_use_locations = path_var_locations instr_uses

let path_used_before_assigned path =
  let assigns = path_assign_locations path in
  let uses    = path_use_locations path in

  uses
  |> List.filter (fun (v, l) ->
      not (List.mem_assoc v assigns) || (List.assoc v assigns >= l))
  |> List.map fst
  |> V_set.of_list

let path_assigns path =
  path
  |> path_assign_locations
  |> List.map fst
  |> V_set.of_list

let connect_path path =
  path
  |> List.map PG.edge
  |> List.fold_left (@) []

let coming p lbl = PG.paths_to p lbl |> List.map connect_path
let going p lbl = PG.paths_from p lbl |> List.map connect_path

let critical_vars p lbl =
  if List.length (going p lbl) = 0 then
    (List.map path_assigns (coming p lbl) |> V_set.unions)
  else
    V_set.inter
      (List.map path_assigns (coming p lbl) |> V_set.unions)
      (List.map path_used_before_assigned (going p lbl) |> V_set.unions)
