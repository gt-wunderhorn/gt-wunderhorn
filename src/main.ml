module JP = Sawja_pack.JProgram
module JB = Javalib_pack.JBasics

let classpath =
  "/home/david/Workspace/path-interpolation/bin:/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/rt.jar"

let make_graph cms =
  let proc_id = ref (0) in
  let cn = fst (JB.cms_split cms) in
  let parse = Parse.parse proc_id classpath cn in
  parse.Parse.cms_lookup cms
  |> Jbir_to_graph.convert parse

let inspect =
  let cn  = JB.make_cn "Test" in
  let cms = JB.make_cms cn JP.main_signature in
  let graph = make_graph cms in

  Print_clauses.print (Graph_to_clauses.translate graph)

let _ =
  Printf.printf "%s\n" inspect
