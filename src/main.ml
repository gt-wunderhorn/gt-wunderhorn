open Shape_state

let _ =
  if (Array.length Sys.argv < 3)
  then
    Printf.eprintf "usage: %s <classpath> <main class name>\n" Sys.argv.(0)
  else
    let classpath = Sys.argv.(1) in
    let class_name = Sys.argv.(2) in
    (* Inspect.print classpath class_name; *)
    (* Inspect.run classpath class_name; *)

    let shapes = Shape.derive classpath class_name in

    let line _ = Printf.eprintf "\n" in

    List.iter
      (fun sh ->
         Printf.eprintf "label: %d\n" sh.label;
         Printf.eprintf "predecessors: ";

         List.iter (Printf.eprintf "%d ") sh.predecessors;
         line ();

         Printf.eprintf "reads: ";
         List.iter (fun r -> Printf.eprintf "%s " r.Field.name) sh.reads;
         line ();

         Printf.eprintf "writes: ";
         List.iter (fun r -> Printf.eprintf "%s " r.Field.name) sh.writes;
         line ();

         line ();
      ) shapes
