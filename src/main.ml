open Shape
let _ =
  if (Array.length Sys.argv < 3)
  then
    Printf.eprintf "usage: %s <classpath> <main class name>\n" Sys.argv.(0)
  else
    let classpath = Sys.argv.(1) in
    let class_name = Sys.argv.(2) in
    (* Inspect.run classpath class_name; *)
    let st = Shape.derive classpath class_name in

    List.iter (fun loc -> Printf.eprintf "%d\n" (nwrites st loc)) (locations st);
    ()


