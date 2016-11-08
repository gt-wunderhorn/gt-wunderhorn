module CZ = Clauses_to_z3
module PG = Program_graph
module G = Graph
module E = Expr

module DT = Z3.Datatype
module FD = Z3.FuncDecl
module S = Z3.Symbol
module B = Z3.Boolean

let ctx = CZ.ctx

let rec constant_list v n =
  if n == 0 then [] else v::(constant_list v (n-1))

type marked_node = PG.lbl * E.var list * Z3.Sort.sort * FD.func_decl list

let mk_ctor s ps ts =
  DT.mk_constructor_s ctx
    s
    (S.mk_string ctx ("is_"^s))
    ps
    (List.map (fun (t) -> Some t) ts)
    (constant_list 0 (List.length ps))

let mk_type s cs = DT.mk_sort_s ctx s cs

let mk_datatype s cs =
  let t = mk_type s (List.map (fun (x) ->
      mk_ctor (fst x)
        (List.map (fun (y) ->
             S.mk_string ctx (Z3.Expr.to_string y)) (snd x))
        (List.map Z3.Expr.get_sort (snd x))) cs) in
  let acs = DT.get_accessors t in
  (t, List.hd acs)

let mk_enum s es =
  if es == [] then (B.mk_sort ctx,[]) else
    (let t = mk_type s (List.map (fun (e) -> mk_ctor e [] []) es) in
     let cs = DT.get_constructors t in
     (t, cs))

let create_datatype lbl vs =
  let const (name, s) = Z3.Expr.mk_const_s ctx name (CZ.sort s) in
  let vs' = List.map const vs in
  mk_datatype ("Data" ^ string_of_int lbl) ["mk-data" ^ string_of_int lbl, vs']

let mark_datatypes graph =
  let mark_node (lbl, vs) =
    let (t, acs) = create_datatype lbl vs in
    (lbl, vs, t, acs) in
  G.map_nodes mark_node graph

let rec has_acsor s t =
  match t with
  | a::_ when s = (S.get_string (FD.get_name a)) -> true
  | _::tl -> has_acsor s tl
  | [] -> false

let rec get_acsor s t =
  match t with
  | a::_ when s = (S.get_string (FD.get_name a)) -> a
  | _::tl -> get_acsor s tl
  | [] -> assert false

let assign (name, _) t vs = Z3.Expr.mk_app ctx (get_acsor name t) [vs]
