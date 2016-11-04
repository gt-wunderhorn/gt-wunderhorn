module JP = Sawja_pack.JProgram
module JB = Javalib_pack.JBasics

module I = Ir
module J_to_i = Jbir_to_ir
module I_to_g = Ir_to_graph
module L = Lang
module LS = Lang_state
module G = Graph
module PG = Program_graph

(* module R = Run_clauses *)

let make_graph classpath cms =
  let proc_id = ref (0) in
  let cn = fst (JB.cms_split cms) in
  let parse = Parse.parse proc_id classpath cn in
  Jbir_to_ir.mk_proc parse cms

  |> Ir_to_graph.procedure
  |> G.splice (fun (e1, n, e2) -> match (e1, e2) with
      | (PG.Body (p, as1), PG.Body (L.True, as2)) ->
         Some (PG.Body (p, as1 @ as2))
      | _ -> None)
  |> G.pinch
    (fun (i, t, e) -> match e with
       | PG.Body (_, []) -> Some t
       | _ -> None)

let inspect classpath class_name =
  let cn  = JB.make_cn class_name in
  let cms = JB.make_cms cn JP.main_signature in
  let graph = make_graph classpath cms in

  let exprs = graph
  |> Simplify.remove_useless_nodes
  |> Path_to_expr.translate
  |> PG.map_body Simplify.remove_simple_assignments
  |> G.pinch (fun (i, t, e) -> match e with
      | PG.Body L.True -> Some t
      | _ -> None)
  (* |> Collapse_expr_graph.collapse *)
  |> fun es -> LS.setup es in

  (* Print_clauses.print exprs |> Printf.printf "%s\n%!"; *)
  (* Printf.eprintf "invoking z3\n%!"; *)

  (* R.run exprs; *)
  ()

let _ =
  if (Array.length Sys.argv < 3)
  then
    Printf.eprintf "usage: %s <classpath> <main class name>\n" Sys.argv.(0)
  else
    let classpath = Sys.argv.(1) in
    let class_name = Sys.argv.(2) in
    inspect classpath class_name
