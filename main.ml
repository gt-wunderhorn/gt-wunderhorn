module JP = Sawja_pack.JProgram
module JB = Javalib_pack.JBasics
module P = Procedure

let classpath =
  "/home/david/Workspace/retry:/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/rt.jar"

let inspect =
  let cn  = JB.make_cn "Test" in
  let cms = JB.make_cms cn JP.main_signature in

  let proc_id = ref (-1) in

  let converter cms =
    Parse.parse proc_id classpath cn cms
    |> P.map A3_to_ir.convert in

  converter cms
  |> Trace.trace converter
  |> Graph_to_clauses.translate
  |> Print_clauses.print

let _ =
  Printf.printf "%s\n" inspect
