module JP = Sawja_pack.JProgram
module JB = Javalib_pack.JBasics

let make_graph classpath cms =
  let proc_id = ref (0) in
  let cn = fst (JB.cms_split cms) in
  let parse = Parse.parse proc_id classpath cn in
  parse.Parse.cms_lookup cms
  |> Jbir_to_graph.convert parse

let inspect classpath class_name =
  let cn  = JB.make_cn class_name in
  let cms = JB.make_cms cn JP.main_signature in
  let graph = make_graph classpath cms in

  Print_clauses.print (Graph_to_clauses.translate graph)

let _ =
  if (Array.length Sys.argv < 3)
  then
    Printf.eprintf "usage: %s <classpath> <main class name>\n" Sys.argv.(0)
  else
    let classpath = Sys.argv.(1) in
    let class_name = Sys.argv.(2) in
    Printf.printf "%s\n" (inspect classpath class_name)
