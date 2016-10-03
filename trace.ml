open Program
module P = Program

type path =
  | Path of linear_instruction list
  | Assertion of variable

let instructions = function
  | Path is     -> is
  | Assertion v -> [Assert v]

let path_variables p =
  Var_set.unions (List.map instruction_variables (instructions p))

module P_graph = Graph.Make(
  struct type node = label;; type edge = path;; end)

type t = P_graph.t

module Label_set = Set_ext.Make(
  struct type t = label;; let compare = compare;; end)

(** A join point is a point where at least two instructions will go after
      being executed. In addition, the first instruction is considered a join
      point. *)
let join_points instrs =
  let num_occurrences x xs = List.filter ((==) x) xs |> List.length in
  let instr_destinations line = function
    | Non_linear (Goto dest)    -> [dest]
    | Non_linear (If (_, dest)) -> [line + 1; dest]
    | _                         -> [line+1] in

  let dests = List.mapi instr_destinations instrs |> List.concat in
  dests
  |> List.filter (fun dest -> num_occurrences dest dests > 1)
  |> Label_set.of_list
  |> Label_set.add 0

(** A split point is a point where multiple instructions might be executed
    after checking some condition. *)
let split_points instrs =
  let record_line_if_split line = function
    | Non_linear (If _) -> Label_set.singleton line
    | Linear (Assert _) -> Label_set.singleton line
    | _                 -> Label_set.empty in

  instrs
  |> List.mapi record_line_if_split
  |> Label_set.unions

(** To trace a program we first find all the critical (join and split) points.
    We initialize partial paths at those locations. Then, we follow each of
    those paths until its end (indicated by another critical point, or the
    program end) is reached. Any degenerate paths are removed. *)
let trace instrs =
  let labels = Label_set.union (join_points instrs) (split_points instrs) in

  let rec start_paths line = match List.nth instrs line with
    | Non_linear (If (e, dest)) -> [(line, dest, Path [Call e]); (line, line+1, Path [Call (mk_not e)])]
    | Non_linear (Goto dest)    -> [(line, dest, Path [])]
    | Linear (Assert v)         -> [(line, line+1, Path []); (line, -1, Assertion v)]
    | Linear i                  -> [(line, line+1, Path [i])] in

  (** A path is complete once it encounters a split, join, or the end of the
      program. It next linear instruction to a partial path until that condition
      is reached. *)
  let rec complete_path path =
    let (first, next, Path is) = path in
    if Label_set.mem next labels || next >= List.length instrs
    then path
    else match List.nth instrs next with
      | Non_linear (Goto line) -> complete_path (first, line, Path is)
      | Non_linear _           -> assert false (* splits already checked *)
      | Linear i               -> complete_path (first, next+1, Path (is @ [i])) in

  Label_set.elements labels
  |> List.map start_paths
  |> List.concat
  |> List.map (fun p -> match p with
      | (f, n, Path is) -> complete_path (f, n, Path is)
      | p -> p)
  |> List.filter (fun (_, _, p) -> instructions p <> [])
  |> P_graph.from_list

let mutated_before graph label =
  graph
  |> P_graph.reaches label
  |> P_graph.edges
  |> List.map instructions
  |> List.concat
  |> List.map instruction_mutables
  |> Var_set.unions

let used_after graph label =
  graph
  |> P_graph.reached_by label
  |> P_graph.edges
  |> List.map instructions
  |> List.concat
  |> List.map instruction_useds
  |> Var_set.unions

let critical_variables graph label =
  Var_set.inter (mutated_before graph label) (used_after graph label)
