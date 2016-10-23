(** `converge` repeatedly applies a function `f` to an argument `x` until
    the change in `x` caused by `f` is stable. *)
let converge is_stable f x =
  let rec converge' x x' =
    if is_stable x x' then x'
    else converge' x' (f x')
  in
  converge' x (f x)

module type Graph_info = sig
  type node
  type edge
end

module Make(T : Graph_info) = struct
  module S = Set_ext.Make(
    struct type t = T.node * T.node * T.edge;; let compare = compare end)

  module N = Set_ext.Make(
    struct type t = T.node;; let compare = compare end)

  type t = T.node * T.node * T.edge

  let empty = S.empty
  let singleton = S.singleton
  let elements = S.elements
  let union = S.union
  let unions = S.unions
  let unions_map = S.unions_map
  let filter = S.filter
  let cardinal = S.cardinal
  let choose = S.choose
  let add = S.add
  let diff = S.diff
  let remove = S.remove
  let of_list = S.of_list
  let iter = S.iter

  let init (i, _, _) = i
  let term (_, t, _) = t
  let edge (_, _, e) = e

  (** Given a graph, get a list of all the edges *)
  let edges g =
    g |> elements |> List.map edge

  (** Given a graph, get a list of all the nodes *)
  let nodes g =
    g |> elements
    |> List.map (fun ce -> [init ce; term ce])
    |> List.concat
    |> N.of_list
    |> N.elements

  (** Find the subgraph where all edges terminate at a given node. *)
  let node_entrances g n = filter (fun (i, t, e) -> t = n) g

  (** Find the subgraph where all edges initiate at a given node. *)
  let node_exits g n = filter (fun (i, t, e) -> i = n) g

  let set_initial (i, t, e) i' = (i', t, e)
  let set_terminal (i, t, e) t' = (i, t', e)

  (** Recursively find all paths from a particular node. The direction of search
      is determined by `selector` and `direction`. Note that each node can only
      be visited exactly once. *)
  let find_paths selector direction g n =
    let visited = ref [] in
    let rec find_paths' g n =
      if List.mem n !visited
      then []
      else
        let local_edges = elements (direction g n) in
        visited := n :: !visited;
        let paths = List.map (fun e -> find_paths' g (selector e)) local_edges in
        let local_paths = List.map (fun p -> [p]) local_edges in
        let add_to_path edge path = path @ [edge] in
        let connect_paths paths edge = List.map (add_to_path edge) paths in
        let extended_paths =
          (List.map2 connect_paths paths local_edges) |> List.concat in
        local_paths @ extended_paths
    in
    find_paths' g n
    |> List.sort_uniq compare

  (** What are the paths that terminate at the given node? *)
  let paths_to = find_paths init node_entrances
  (** What are the paths that initiate at the given node? *)
  let paths_from = find_paths term node_exits

  (** Find the set of edges which are eventually connected to a starting node
      in a particular direction. *)
  let follow selector direction g n =
    find_paths selector direction g n
    |> List.concat
    |> of_list

  (** Which edges eventually reach the node? *)
  let reaches = follow init node_entrances
  (** Which edges eventually are reached from the node? *)
  let reached_by = follow term node_exits


  (** We define a `bridge` node as being one which only has a single input edge
      and a single output edge. `simplify` removes bridge nodes by combining
      the input and output edge using `combine`. *)
  let merge_bridges (f : (T.edge * T.edge * T.node) -> T.edge option) =
    let simplify' g =
      let g' = ref g in

      let simp n =
        let g = !g' in
        let ps = node_entrances g n in
        let cs = node_exits g n in
        if cardinal ps = 1 && cardinal cs = 1
        then
          (* When we've found a bridge, we removed both bridge edges and add
             a new edge which combines the edges of the two removed edges. *)
          let p = choose ps in
          let c = choose cs in

          match f (edge p, edge c, n) with
          | None -> ()
          | Some e ->
            g' := remove p !g';
            g' := remove c !g';
            g' := add (init p, term c, e) !g'
      in
      g |> nodes |> List.iter simp;
      !g'
    in
    converge (=) simplify'

(** Two nodes are strictly connected if one has only one outgoing edge which goes
    to the other and the other has only one incoming edge which comes from the
    first.

    It is sometimes useful to be able to combine strictly connected nodes into
    a single node. The user provides a function which takes in two nodes and the
    edge and might return the combined node. If the user function combines the
    nodes, then the original nodes and edge are removed, the new node is added,
    and other involved edges are rewired to the new node. *)
  let merge_strictly_connected (f : t -> T.node option) =
    let merge g =
      let g' = ref g in
      let simp (ce : t) =
        let up = node_entrances !g' (term ce) in
        let down = node_exits !g' (init ce) in

        if cardinal down = 1 && cardinal up = 1
        then match f ce with
          | Some n ->
            let incoming = node_entrances !g' (init ce) in
            let outgoing = node_exits !g' (term ce) in
            g' := remove ce !g';
            iter (fun ce ->
                g' := remove ce !g';
                g' := add (init ce, n, edge ce) !g'
              ) incoming;
            iter (fun ce ->
                g' := remove ce !g';
                g' := add (n, term ce, edge ce) !g'
              ) outgoing
          | None -> ()
      in
      g |> elements |> List.iter simp;
      !g'
    in
    converge (=) merge

  let display sn se g =
    g |> elements
    |> List.map (fun (i, t, e) -> String.concat " " [sn i; sn t; se e])
    |> String.concat "\n"
end
