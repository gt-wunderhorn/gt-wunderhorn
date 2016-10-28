module JP = Sawja_pack.JProgram
module JB = Javalib_pack.JBasics

module I = Ir
module J_to_i = Jbir_to_ir
module I_to_g = Ir_to_graph
module L = Lang
module LS = Lang_state
module G = L.PG

module R = Run_clauses
module P = Print_clauses

let make_graph classpath cms =
  let proc_id = ref (0) in
  let cn = fst (JB.cms_split cms) in
  let parse = Parse.parse proc_id classpath cn in
  Jbir_to_ir.mk_proc parse cms

  |> Ir_to_graph.procedure
  |> G.merge_bridges (fun (e1, e2, n) -> Some (e1 @ e2))
  |> G.merge_strictly_connected
    (fun (i, t, e) -> if e = [] then Some t else None)

let inspect classpath class_name =
  let cn  = JB.make_cn class_name in
  let cms = JB.make_cms cn JP.main_signature in
  let graph = make_graph classpath cms in

  graph
  |> Simplify.remove_useless_nodes
  |> Graph_to_clauses.translate
  |> List.map Simplify.remove_simple_assignments
  |> fun es -> LS.setup es
  (* |> Print_clauses.print |> Printf.printf "%s\n" *)
  |> Run_clauses.run

let _ =
  if (Array.length Sys.argv < 3)
  then
    Printf.eprintf "usage: %s <classpath> <main class name>\n" Sys.argv.(0)
  else
    let classpath = Sys.argv.(1) in
    let class_name = Sys.argv.(2) in
    inspect classpath class_name
