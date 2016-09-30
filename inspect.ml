open Convert
open Parse

let classpath =
  "/home/david/Workspace/retry:/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/rt.jar"

let inspect program =
  Parse.parse classpath "Test"
  |> Convert.convert
  |> Trace.trace
  |> Translate.translate
  |> Interpret.interpret
