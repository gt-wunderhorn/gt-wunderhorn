type t = (int, unit) Graph.t

let graph =
  Graph.of_conns
    [ 1, 2, ()
    ; 2, 3, ()
    ; 3, 4, ()
    ; 4, 5, ()

    ; 2, 6, ()
    ; 6, 7, ()
    ; 7, 3, ()

    ; 3, 8, ()
    ; 8, 9, ()
    ; 9, 4, ()
    ]

let walks = Graph.walks_to graph 5

let print_walk walk =
  List.iter (fun (n1, n2, _) -> Printf.eprintf "%d %d\n" n1 n2) walk;
  Printf.eprintf "\n"

let _ =
  List.iter print_walk walks
