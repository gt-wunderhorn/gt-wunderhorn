open Abstr_lang

type path =
  | Predicated of line * expr * instr list
  | Determined of instr list

let paths instrs =
  (** A join point is an instruction that is reachable by more than one other
      instruction.

      We use an array where the index is the line count and the value is the
      number of accesses. We start by assuming each line is accessed once.
      Then, control flow instructions change these values. The instructions are
      filtered for only those whose corresponding array value is greater than 1. *)
  let is_join_point instr =
    let join_points =
      let line_counts = Array.make (List.length instrs) 1 in
      let line_count = Array.get line_counts in
      let change line f = Array.set line_counts line (f (line_count line)) in
      let alter_line_counts (Instr (Line ln, instr)) = match instr with
        | Goto (Line num)  ->
          change (ln+1) (fun x -> x-1);
          change num      (fun x -> x+1)
        | If (_, Line num) ->
          change num (fun x -> x+1)
        | _ -> () in
      List.iter alter_line_counts instrs;
      List.filter (fun (Instr (Line ln, _)) -> line_count ln > 1) instrs in
    List.mem instr join_points in

  (** A split point is an instruction where the flow of control could diverge:
      In other words, an `If`. *)
  let is_split_point (Instr (_, instr)) = match instr with
    | If _ -> true
    | _    -> false in

  (** An instruction is the end of a path if it is a split or a join point. *)
  let is_path_end instr = is_split_point instr || is_join_point instr in

  (** Starting at a particular line, build up a list of instructions until a
      path end is found. *)
  let rec follow instr : instr list =
    let (Instr (Line ln, instr')) = instr in
    if is_path_end instr
    then [instr]
    else match instr' with
      | Goto (Line l) -> follow_ln l
      | If _          -> []
      | Return e      -> [instr]
      | Assert v      -> [instr]
      | _             -> instr :: follow_ln (ln+1)
  and follow_ln ln = follow (List.nth instrs ln) in

  (** Look at a particular instruction. Find any paths starting at that
      location. Only the entry point (line 0) or path ends can possibly have
      paths originating at them. *)
  let path_from (instr : instr) : path list =
    let (Instr (Line ln, instr')) = instr in
    if ln == 0
    (** The entry point is always the beginning of a path *)
    then [Determined (follow instr)]
    else if is_path_end instr
    then match instr' with
      (** `If` branches generate two paths: One that follows the if and one that
          continues uninterrupted. The first path is subject to the branch
          condition and the second is subject to the opposite. *)
      | If (e, (Line l)) ->
        [ Predicated (Line ln, e,     follow_ln l)
        ; Predicated (Line ln, Not e, follow_ln (ln+1))]

      (** A goto which is also a path end indicates that a new path begins at its target. *)
      | Goto (Line l)    -> [Determined (follow_ln l)]

      (** An assert can only be the start of a path which only includes itself. *)
      | Assert v         -> [Determined [instr]]

      (** Any other instruction which starts a path is built up by following. *)
      | _                -> [Determined (follow instr)]
    else [] in

  List.map path_from instrs |> List.concat
