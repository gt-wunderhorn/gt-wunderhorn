(** A `Path` is a sequence of instructions which, once entered, should execute
    from beginning to end. *)
type path =
  | Predicated of line * line * expr * instr list
  | Determined of instr list

val paths : Abstr_lang.instr list -> path list

val first : path -> line
val last  : path -> line

val reachable : path -> path list

val variables : path -> Abstr_lang.variable list
