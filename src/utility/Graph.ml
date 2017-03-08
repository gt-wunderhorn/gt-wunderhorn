module Set = Core.Std.Set.Poly

type ('n, 'e) t = ('n * 'n * 'e) Set.t

let empty = Set.empty

let conns g = Set.to_list g
let nodes g =
  g
  |> Set.elements
  |> List.map (fun (p, c, e) -> [p; c])
  |> List.concat

let edges g = Set.to_list (Set.map ~f:(fun (_,_,x) -> x) g)

let add (n1, n2, e) g =
  Set.add g (n1, n2, e)

let of_conns conns =
  let add g c = add c g in
  List.fold_left add empty conns

let singleton conn = of_conns [conn]

let id x = x

let map nf ef g =
  let conn (n1, n2, e) = (nf n1, nf n2, ef e) in
  Set.map ~f:conn g
let map_nodes nf g = map nf id g
let map_edges ef g = map id ef g
let map_conns f g = Set.map ~f:f g

let discard x = ()
let iter nf ef g =
  let conn (n1, n2, e) =
    nf n1; nf n2; ef e in
  Set.iter ~f:conn g
let iter_nodes nf g = iter nf discard g
let iter_edges ef g = iter discard ef g
let iter_conns f g = Set.iter ~f:f g

let union = Set.union
let diff  = Set.diff
let equal = Set.equal

let unions g = List.fold_left union empty g
let unions_map f xs = unions (List.map f xs)

let filter_nodes p g =
  let conn (n1, n2, _) = p n1 && p n2 in
  Set.filter ~f:conn g

let filter_edges p g =
  let conn (_, _, e) = p e in
  Set.filter ~f:conn g

let filter_conns p g = Set.filter ~f:p g

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
          |> add (p, c, e)
    done;
    if not !changed then finished := true;
    !g'
  in
  Algorithm.converge (fun _ _ -> !finished) splice' g

(** Recursively find all walks from a particular node. The direction of search
    is determined by `selector` and `direction`. Note that each node can only
    be appear exactly once in a given walk. *)
let find_walks_p p selector direction g n =
  let rec follow so_far conn =
    let nodes = Set.of_list (List.concat (List.map (fun (n1, n2, e) -> [n1;n2]) so_far)) in
    if Set.mem nodes (selector conn) || p (selector conn)
    then
      [conn :: so_far]
    else
      let locals = direction g (selector conn) in
      match locals with
      | [] -> [conn :: so_far]
      | ls -> List.concat (List.map (follow (conn :: so_far)) ls)
  in
  List.concat (List.map (follow []) (direction g n))

let find_walks selector direction g n =
  find_walks_p (fun _ -> false) selector direction g n

let walks_between g n1 n2 =
  find_walks_p (fun n -> n = n1) init conns_to g n2

(** What are the walks that terminate at the given node? *)
let walks_to g n = find_walks init conns_to g n
(** What are the walks that initiate at the given node? *)
let walks_from g n = find_walks term conns_from g n
