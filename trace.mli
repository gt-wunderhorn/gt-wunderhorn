(** A path is a linear set of program instructions which will execute as a
    block. *)
type path =
  | Path of Program.label * Program.label * Program.linear_instruction list
  | Assertion of Program.label * Program.variable

type variable_set = Program.Var_set.t

(** What is the label which describes where the path begins? *)
val initial_label : path -> Program.label

(** What is the label which describes where the path ends? *)
val terminal_label : path -> Program.label

(** What are the instructions on the program path? Note that only linear
    instructions are possible. All non-linear instructions have been removed
    by tracing. *)
val instructions : path -> Program.linear_instruction list


(** A trace is the list of all program paths possible. Note that the choice made
    is that every instruction in the original program should appear exactly once
    in the trace. *)
type t = path list

(** Given a list of program instructions, find the individual paths through
    the program.

    Note that in the case of a branch, new non-linear instructions are added.
    An instruction like `If e destination` generates two paths. One path
    (which includes the instructions at destination) gets a `Call e` instruction.
    The other (which includes instructions immediately after the `If`) gets a
    `Call (Not e)` instuction. *)
val trace : Program.instruction list -> t

val path_variables : path -> variable_set


(** The variables which are used on any path which leads to a label. *)
val ancestral_variables : t -> Program.label -> variable_set

(** The variables which are used on any path which follows a label. *)
val descendant_variables : t -> Program.label -> variable_set

(** The variables which are mutated on any path which leads to a label. *)
val ancestral_mutables : t -> Program.label -> variable_set

(** The variables which are mutated on any path which follows to a label. *)
val descendant_mutables : t -> Program.label -> variable_set

(** The critical variables across a label are those which were mutated before
    the label and used after the label. They are considered critical because
    they paths which occur after the label care about what happened before. *)
val critical_variables : t -> Program.label -> variable_set
