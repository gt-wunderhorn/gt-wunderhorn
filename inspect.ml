module JP = Sawja_pack.JProgram
module JB = Javalib_pack.JBasics
module P = Procedure

let classpath =
  "/home/david/Workspace/retry:/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/rt.jar"

let inspect program =
  let cn  = JB.make_cn "Test" in
  let cms = JB.make_cms cn JP.main_signature in

  let proc_id = ref (-1) in

  let converter cms =
    Parse.parse proc_id classpath cn cms
    |> P.map Convert.convert in

  converter cms
  |> Trace.trace converter
  |> Translate.translate
  |> Interpret.interpret
