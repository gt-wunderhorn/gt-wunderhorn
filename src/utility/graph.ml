module Set = Core.Std.Set.Poly

type ('n, 'e) conn = ('n * 'n * 'e)
type ('n, 'e) t =
  { nodes : 'n Set.t
  ; conns : (('n, 'e) conn) Set.t
  }

let empty = { nodes = Set.empty ; conns = Set.empty }

let conns g = Set.to_list g.conns
let nodes g = Set.to_list g.nodes
let edges g = Set.to_list (Set.map ~f:(fun (_,_,x) -> x) g.conns)

let add_node n g = { g with nodes = Set.add g.nodes n }
let add_conn (n1, n2, e) g =
  let g' = add_node n1 g |> add_node n2 in
  { g' with conns = Set.add g'.conns (n1, n2, e) }

let of_conns conns =
  let add g c = add_conn c g in
  List.fold_left add empty conns

let singleton conn = of_conns [conn]

let map nf ef g =
  let conn (n1, n2, e) = (nf n1, nf n2, ef e) in
  { nodes       = Set.map ~f:nf g.nodes
  ; conns = Set.map ~f:conn g.conns
  }

let id x = x
let map_nodes nf g = map nf id g
let map_edges ef g = map id ef g

let union g1 g2 =
  { nodes = Set.union g1.nodes g2.nodes
  ; conns = Set.union g1.conns g2.conns
  }

let unions g = List.fold_left union empty g
let unions_map f xs = unions (List.map f xs)

let filter_nodes p g =
  let conn (n1, n2, _) = p n1 && p n2 in
  { nodes = Set.filter ~f:p g.nodes
  ; conns = Set.filter ~f:conn g.conns }

let filter_edges p g =
  let conn (_, _, e) = p e in
  { g with conns = Set.filter ~f:conn g.conns }

let filter_conns p g =
  { g with conns = Set.filter ~f:p g.conns }

let parents g n =
  List.filter (fun (_, c, _) -> c = n) (conns g)
  |> List.map (fun (p, _, _) -> p)

let children g n =
  List.filter (fun (p, _, _) -> p = n) (conns g)
  |> List.map (fun (_, c, _) -> c)

let conns_to g n =
  List.filter (fun (_, c, _) -> c = n) (conns g)

let conns_from g n =
  List.filter (fun (p, _, _) -> p = n) (conns g)

let entrances g n =
  List.filter (fun (_, c, _) -> c = n) (conns g)
  |> List.map (fun (_, _, e) -> e)

let exits g n =
  List.filter (fun (p, _, _) -> p = n) (conns g)
  |> List.map (fun (_, _, e) -> e)

let init (p, _, _) = p
let term (_, c, _) = c
let edge (_, _, e) = e

let pinch f g =
  let finished = ref false in
  let pinch' g =
    let pinchable = List.filter
        (fun (p, c, _) -> List.length (children g p) = 1 &&
                          List.length (parents g c) = 1) (conns g)
                    |> Array.of_list in
    let changed = ref false in
    let idx = ref 0 in
    let g' = ref g in
    while (not !changed && !idx < Array.length pinchable) do
      let (p, c, e) = pinchable.(!idx) in
      idx := !idx + 1;
      g' := match f (p, c, e) with
        | None   -> !g'
        | Some n -> changed := true;
          filter_conns (fun conn -> conn <> (p, c, e)) !g'
          |> map_nodes (fun n' -> if n' = p || n' = c then n else n')
    done;
    if not !changed then finished := true;
    !g'
  in
  Algorithm.converge (fun _ _ -> !finished) pinch' g

let splice f g =
  let finished = ref false in
  let splice' g =
    let spliceable = List.filter
        (fun n -> List.length (parents g n) = 1 &&
                  List.length (children g n) = 1) (nodes g)
                     |> Array.of_list in
    let changed = ref false in
    let idx = ref 0 in
    let g' = ref g in
    while (not !changed && !idx < Array.length spliceable) do
      let n = spliceable.(!idx) in
      let (p, _, e1) = List.hd (conns_to g n) in
      let (_, c, e2) = List.hd (conns_from g n) in
      idx := !idx + 1;
      g' := match f (e1, n, e2) with
        | None   -> !g'
        | Some e -> changed := true;
          filter_nodes (fun n' -> n' <> n) !g'
          |> add_conn (p, c, e)
    done;
    if not !changed then finished := true;
    !g'
  in
  Algorithm.converge (fun _ _ -> !finished) splice' g

(** Recursively find all walks from a particular node. The direction of search
    is determined by `selector` and `direction`. Note that each node can only
    be visited exactly once. *)
let find_walks selector direction g n =
  let visited = ref [] in
  let rec find_walks' g n =
    if List.mem n !visited
    then []
    else
      let local_edges = direction g n in
      visited := n :: !visited;
      let walks = List.map (fun e -> find_walks' g (selector e)) local_edges in
      let local_walks = List.map (fun p -> [p]) local_edges in
      let add_to_path edge path = path @ [edge] in
      let connect_walks walks edge = List.map (add_to_path edge) walks in
      let extended_walks =
        (List.map2 connect_walks walks local_edges) |> List.concat in
      local_walks @ extended_walks
  in
  find_walks' g n
  |> Set.of_list
  |> Set.elements

(** What are the walks that terminate at the given node? *)
let walks_to g n = find_walks init conns_to g n
(** What are the walks that initiate at the given node? *)
let walks_from g n = find_walks term conns_from g n

