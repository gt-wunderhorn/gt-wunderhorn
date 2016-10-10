open Ir

type path =
  | Path of linear_instr list
  | Assertion of expr

let instructions = function
  | Path is     -> is
  | Assertion v -> [Assert v]

let path_vars p =
  Var_set.unions (List.map instr_vars (instructions p))

module P_graph = Graph.Make(
  struct type node = string;; type edge = path;; end)

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
    | Non_linear (If _)     -> Label_set.singleton line
    | Non_linear (Invoke _) -> Label_set.singleton line
    | Linear (Assert _)     -> Label_set.singleton line
    | _                     -> Label_set.empty in

  instrs
  |> List.mapi record_line_if_split
  |> Label_set.unions

(** To trace a program we first find all the critical (join and split) points.
    We initialize partial paths at those locations. Then, we follow each of
    those paths until its end (indicated by another critical point, or the
    program end) is reached. Any degenerate paths (no instructions) are removed. *)
let rec trace prog =
  let instrs = prog.Ir.content in
  let labels = Label_set.union (join_points instrs) (split_points instrs) in

  let mk_label num = prog.Ir.id ^ "_" ^ string_of_int num in

  (** A path is complete once it encounters a split, join, or the end of the
      program. It next linear instruction to a partial path until that condition
      is reached. *)
  let rec complete path =
    let (first, next, is) = path in

    let rec complete_goto next =
      if next >= List.length instrs
      then P_graph.singleton (first, mk_label next, Path is)
      else match List.nth instrs next with
        | Non_linear (Goto line) -> complete_goto line
        | _ -> P_graph.singleton (first, mk_label next, Path is)
    in

    if next >= List.length instrs
    then P_graph.singleton (first, mk_label next, Path is)
    else if Label_set.mem next labels
    then match List.nth instrs next with
      | Non_linear (Goto line) -> complete_goto line
      | _ -> P_graph.singleton (first, mk_label next, Path is)
    else match List.nth instrs next with
      | Non_linear (Goto line) -> complete (first, line, is)
      | Non_linear (Return v)  ->
        P_graph.singleton (first, Ir.exit_label prog, Path (is @ [Assign (Ir.ret_var prog, v)]))
      | Non_linear _           -> assert false (* splits already checked *)
      | Linear i               -> complete (first, next+1, (is @ [i])) in

  let subgraph_from line = match List.nth instrs line with
    | Non_linear (If (e, dest)) ->
      P_graph.merge
        (complete (mk_label line, dest, [Call e]))
        (complete (mk_label line, line+1, [Call (mk_not e)]))
    | Non_linear (Goto dest) ->
      complete (mk_label line, dest, [])
    | Linear (Assert v) ->
      P_graph.merge
        (complete (mk_label line, line+1, []))
        (P_graph.singleton (mk_label line, mk_label (-1), Assertion v))
    | Linear i ->
      complete (mk_label line, line+1, [i])
    | Non_linear (Invoke (v, proc, args)) ->
      P_graph.merges
        [ trace proc
        ; P_graph.singleton (
            mk_label line,
            Ir.entry_label proc,
            Path (List.map2 (fun param arg -> Assign (param, arg)) proc.Ir.params args))
        ; complete (
            Ir.exit_label proc,
            line+1,
            [Assign (v, Ir.Var (Ir.ret_var proc))]) ]
    | _ -> P_graph.empty
  in

  Label_set.elements labels
  |> List.map subgraph_from
  |> P_graph.merges
  |> P_graph.filter (fun p -> instructions p <> [])

let mutated_before graph label =
  graph
  |> P_graph.reaches label
  |> P_graph.edges
  |> List.map instructions
  |> List.concat
  |> List.map instr_mutables
  |> Var_set.unions

let used_after graph label =
  graph
  |> P_graph.reached_by label
  |> P_graph.edges
  |> List.map instructions
  |> List.concat
  |> List.map instr_useds
  |> Var_set.unions

let critical_vars graph label =
  Var_set.inter (mutated_before graph label) (used_after graph label)
