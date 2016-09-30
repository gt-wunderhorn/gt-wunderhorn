open Program

type path =
  | Path of label * label * linear_instruction list
  | Assertion of label * variable

type t = path list
type variable_set = Program.Var_set.t

(** A join point is a point where at least two instructions will go after
      being executed. In addition, the first instruction is considered a join
      point. *)
let join_points instrs =
  let num_occurrences x xs = List.filter ((==) x) xs |> List.length in
  let instr_destinations line = function
    | Linear _                  -> [line + 1]
    | Non_linear (Goto dest)    -> [dest]
    | Non_linear (If (_, dest)) -> [line + 1; dest] in

  let dests = List.mapi instr_destinations instrs |> List.concat in
  dests
  |> List.filter (fun dest -> num_occurrences dest dests > 1)
  |> List.sort_uniq compare
  |> fun xs -> 0 :: xs

(** A split point is a point where multiple instructions might be executed
    after checking some condition. *)
let split_points instrs =
  let record_split line = function
    | Non_linear (If _) -> [line]
    | Linear (Assert _) -> [line]
    | _                 -> [] in

  instrs
  |> List.mapi record_split
  |> List.concat

(** To trace a program we first find all the critical (join and split) points.
    We initialize partial paths at those locations. Then, we follow each of
    those paths until its end (indicated by another critical point, or the
    program end) is reached. Any degenerate paths are removed. *)
let trace instrs =
  let joins  = join_points  instrs in
  let splits = split_points instrs in

  (** Each split begins two paths which start with their respective conditions. *)
  let split_starts line = match List.nth instrs line with
    | Non_linear (If (e, dest)) ->
      [ Path (line, dest, [Call e])
      ; Path (line, line+1, [Call (mk_not e)])
      ]
    | Linear (Assert v) ->
      [ Path (line, line+1, [])
      ; Assertion (line, v)
      ]
    | _ -> assert false (* splits are only ifs and asserts *) in

  (** Each join begins a path. The join begins with the first linear instruction
      in the path iff it does not encounter a split or join beforehand. *)
  (** TODO If a join starts with a goto which immediately travels to another
      join, then the path starting with the first join will incorrectly get
      the first instruction of the next path *)
  let rec join_original_instrs line = match List.nth instrs line with
    | Non_linear (If _)      -> (line, [])
    | Non_linear (Goto line) -> join_original_instrs line
    | Linear (Assert _)      -> (line, [])
    | Linear i -> (line + 1, [i]) in
  let join_starts line =
    let (next, instrs) = join_original_instrs line in
    Path (line, next, instrs) in

  (** A path is complete once it encounters a split, join, or the end of the
      program. It next linear instruction to a partial path until that condition
      is reached. *)
  let rec complete_path path =
    let Path (first, next, is) = path in
    if List.mem next joins
    || List.mem next splits
    || next >= List.length instrs
    then path
    else match List.nth instrs next with
      | Non_linear (Goto line) -> complete_path (Path (first, line, is))
      | Non_linear _           -> assert false (* splits already checked *)
      | Linear (Assert _)      -> assert false (* splits already checked *)
      | Linear i               ->
        complete_path (Path (first, next+1, is @ [i])) in

  (** A degenerate path is one which has no instructions. *)
  let non_degenerate = function
    | Path (_, _, is) -> is <> []
    | _               -> true in

  (** All paths in a program begin at a split or a join. *)
  (List.map join_starts joins) @ (List.map split_starts splits |> List.concat)
  |> List.map (fun p -> match p with
      | Path (f, n, is) -> complete_path (Path (f, n, is))
      | p               -> p)
  |> List.filter non_degenerate

let initial_label = function
  | Path (f, _, _)   -> f
  | Assertion (f, _) -> f
let terminal_label = function
  | Path (_, n, _)   -> n
  | Assertion (_, _) -> -1

let instructions = function
  | Path (_, _, is)  -> is
  | Assertion (_, v) -> [Assert v]


let find_reachable_in_direction start_label end_label paths label =
  let is_reachable label path = label == start_label path in
  let rec loop label acc =
    let initial_reachables path acc = loop (end_label path) acc in
    let local_reachables =
      paths
      |> List.filter (is_reachable label)
      |> List.filter (fun path -> not (List.mem path acc))
    in
    List.fold_right initial_reachables local_reachables (local_reachables @ acc) in
  loop label []

let ancestors   = find_reachable_in_direction terminal_label initial_label
let descendants = find_reachable_in_direction initial_label terminal_label

let path_variables_of_type variable_finder path =
  instructions path
  |> Var_set.unions_map variable_finder

let path_variables = path_variables_of_type instruction_variables

let variables variable_finder direction paths label =
  direction paths label
  |> Var_set.unions_map (path_variables_of_type variable_finder)

let ancestral_variables  = variables instruction_variables ancestors
let descendant_variables = variables instruction_variables descendants
let ancestral_mutables   = variables instruction_mutables  ancestors
let descendant_mutables  = variables instruction_mutables  descendants

let critical_variables paths label =
  Var_set.inter
    (ancestral_mutables paths label)
    (descendant_variables paths label)
