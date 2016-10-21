(** A labeller generates new labels to represent different objects. It keeps a
    record of labels that have already been assigned, so that if the same object
    is passed in more than once, it will retain its original label. *)

module Make (C : Comparable.T) = struct
  module L_map = Map.Make(C)
  type t =
    { prefix : string
    ; mutable current_label : int
    ; mutable assignments : string L_map.t
    }

  let mk prefix =
    { prefix = prefix
    ; current_label = 0
    ; assignments = L_map.empty
    }

  let label t x =
    if not (L_map.mem x t.assignments)
    then
      let new_label = t.prefix ^ string_of_int t.current_label in
      t.current_label <- t.current_label + 1;
      t.assignments <- L_map.add x new_label t.assignments;
      new_label
    else
      L_map.find x t.assignments
end
