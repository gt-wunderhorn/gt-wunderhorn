module AL = Abstr_lang

(** Find those instructions which are reachable by more than one other
    instruction.

    We use an array where the index is the line count and the value is the
    number of accesses. We start by assuming each line is accessed once.
    Then, control flow instructions change these values. The instructions are
    filtered for only those whose corresponding array value is greater than 1. *)
let join_points instrs =
  let line_counts = Array.make (List.length instrs) 1 in
  let line_count = Array.get line_counts in
  let change line f = Array.set line_counts line (f (line_count line)) in
  let alter_line_counts (line, instr) = match instr with
    | AL.Assign _            -> ()
    | AL.Assert _            -> ()
    | AL.Goto (AL.Line num)  ->
      change (line+1) (fun x -> x-1);
      change num      (fun x -> x+1)
    | AL.If (_, AL.Line num) ->
      change num (fun x -> x+1) in
  let lined_instrs = List.mapi (fun line instr -> (line, instr)) instrs in
  List.iter alter_line_counts lined_instrs;
  let lined_jps = List.filter (fun (line, _) -> line_count line > 1) lined_instrs in
  List.map snd lined_jps

(** Split points are just those points which generate multiple control paths. In
    the abstract instruction set, only `If` is capable of doing this. *)
let split_points instrs =
  let is_split = function
    | AL.If _ -> true
    | _       -> false in
  List.filter is_split instrs

(** A control path is a set of instructions which starts at some point in the
    program and ends at the nearest join point. A path results in one horn
    clause. On the left of this horn clause are the conditions implied by each
    instruction in the path. On the right is a relation applied to relevant
    variables.

    There are a few important notions involved in building up a control path
    clause:
    1. A program always has a minimum of 1 control path.
    2. Multiple control paths only emerge as the result of `If` instructions.
    3. Any mutation (via assignment) means that a program variable needs to be
       renamed. This new name should be used for each appearance of that program
       variable from then on.
    4. The resultant relation only needs to be bound over those variables which
       a. were assigned to
       b. appear later in the program *)

module Var_map = Map.Make(struct
    type t = AL.variable;;
    let compare (AL.Variable x) (AL.Variable y) = compare x y
end)
type var_map = AL.variable Var_map.t

let path instrs =
  let instr_condition = function
    | Assign (v, e) -> ()
    | If (e, l)     -> ()
    | Goto l        -> ()
    | Assert v      -> () in
  assert false

let create_horn_program instrs = assert false
