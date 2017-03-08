let usage _ =
  Printf.eprintf "usage: %s <classpath> <main class name> [print|run]\n" Sys.argv.(0)

let _ =
  if (Array.length Sys.argv < 4)
  then
    usage ()
  else
    let classpath = Sys.argv.(1) in
    let class_name = Sys.argv.(2) in

    let exprs = Inspect.inspect classpath class_name in

    if Sys.argv.(3) = "print"
    then
      let p = PrintClauses.print exprs in
      Core.Std.Out_channel.write_all "example.z3" ~data:p

    else if Sys.argv.(3) = "run"
    then ClausesToZ3.run exprs

    else
      ( Printf.eprintf "unknown run type: %s\n" Sys.argv.(3)
      ; usage ()
      )

