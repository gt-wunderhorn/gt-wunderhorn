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
    type t = variable;;
    let compare (Variable x) (Variable y) = compare x y
  end)
type var_map = variable Var_map.t

let create_horn_program instrs = assert false
