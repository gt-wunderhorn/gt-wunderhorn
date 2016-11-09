open Ingestion

let _ =
  if (Array.length Sys.argv < 3)
  then
    Printf.eprintf "usage: %s <classpath> <main class name>\n" Sys.argv.(0)
  else
    let classpath = Sys.argv.(1) in
    let class_name = Sys.argv.(2) in
    (* Inspect.print classpath class_name; *)
    (* Inspect.run classpath class_name; *)

    let context = derive classpath class_name in

    let line _ = Printf.eprintf "\n" in

    List.iteri
      (fun i bb ->
         Printf.eprintf "label: %d\n" i;
         Printf.eprintf "predecessors: ";

         List.iter (fun bb -> Printf.eprintf "%s " (location_name bb)) (predecessors context bb);
         line ();

         Printf.eprintf "reads: ";

         (* List.iter (fun r -> Printf.eprintf "%s " r.Field.name) bb.reads; *)
         (* line (); *)

         (* Printf.eprintf "writes: "; *)
         (* List.iter (fun r -> Printf.eprintf "%s " r.Field.name) bb.writes; *)
         line ();

         line ();
      ) (locations context);
    Printf.eprintf "%s\n" (location_name (final_location context));
