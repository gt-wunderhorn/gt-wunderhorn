module JP = Sawja_pack.JProgram
module JB = Javalib_pack.JBasics

let classpath =
  "/home/david/Workspace/path-interpolation/bin:/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/rt.jar"

let inspect =
  let cn  = JB.make_cn "Test" in
  let cms = JB.make_cms cn JP.main_signature in

  let proc_id = ref (-1) in

  let parse cms =
    Parse.parse proc_id classpath cn cms in

  parse cms
  |> Ir.map (Jbir_to_ir.convert parse)
  |> Trace.trace
  |> Graph_to_clauses.translate
  |> Print_clauses.print

let _ =
  Printf.printf "%s\n" inspect
