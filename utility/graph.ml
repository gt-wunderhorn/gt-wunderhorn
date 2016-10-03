module Make (T : Graph_info.T) = struct
  type connected_edge =
    { initial  : T.node
    ; terminal : T.node
    ; content  : T.edge
    }

  module CE_set = Set_ext.Make(
    struct type t = connected_edge;; let compare = compare end)

  type t = CE_set.t

  let empty = CE_set.empty
  let merge x y = CE_set.union x y

  let add initial terminal content =
    CE_set.add ({ initial; terminal; content })

  let from_list c_edges =
    c_edges
    |> List.map (fun (initial, terminal, content) ->
        { initial; terminal; content })
    |> CE_set.of_list

  let c_edges = CE_set.elements

  let nodes g =
    g |> c_edges
    |> List.map (fun e -> [e.initial; e.terminal])
    |> List.concat
    |> List.sort_uniq compare

  let edges g =
    g |> c_edges
    |> List.map (fun e -> e.content)

  let connected_edges g =
    g |> c_edges
    |> List.map (fun e -> (e.initial, e.terminal, e.content))
  (** Find the subgraph which is reachable using the node selector to compare
      each edge against the given node.

      The algorithm maintains a `visited` list which indicates whether or not a
      particular node has already been visited. This is to prevent the algorithm
      from hanging when there are graphs which loop. *)
  let reachable node_selector node graph =
    let visited = ref [] in
    let rec reachable node =
      if List.mem node !visited
      then CE_set.empty
      else let neighbors = CE_set.filter (fun ce -> node_selector ce = node) graph in
        visited := node :: !visited;
        CE_set.union
          neighbors
          (nodes neighbors
           |> List.map reachable
           |> CE_set.unions) in
    reachable node

  let reached_by = reachable (fun e -> e.initial)
  let reaches = reachable (fun e -> e.terminal)

  let display print_node print_edge g =
    g |> c_edges
    |> List.map (fun e -> String.concat " "
                    [ print_node e.initial
                    ; print_node e.terminal
                    ; print_edge e.content ])
    |> String.concat "\n"
end

(* let my_edges = *)
(*   [ (0, 1, 'a') *)
(*   ; (1, 2, 'c') *)
(*   ; (1, 3, 'b') *)
(*   ; (2, 0, 'd') *)
(*   ; (3, 4, 'e') *)
(*   ; (3, 5, 'f') *)
(*   ; (4, 6, 'g') *)
(*   ; (5, 6, 'h') ] *)

(* module CGraph = Make( *)
(*   struct type node = int;; type edge = char end) *)

(* let g = CGraph.from_list my_edges *)

(* let print g = Printf.printf "%s\n" (CGraph.display string_of_int Char.escaped g) *)

(* let _ = print g *)
