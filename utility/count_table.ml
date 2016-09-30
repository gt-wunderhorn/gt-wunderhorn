(** A count table keeps a counter for each element in the domain. A new table
    can be created with `mk`, passing in the domain. Each element in the domain
    will start with a count of 0. This count can be incremented with `increment`
    and accessed with `get`. *)

module Make (C : Comparable.T) = struct
  module Underlying = Map.Make(C)

  type t = C.t Underlying.t

  let mk indices =
    let add_entry table index = Underlying.add index 0 table in
    List.fold_left add_entry Underlying.empty indices

  let get table index =
    Underlying.find index table

  let increment table index =
    Underlying.add index ((get table index)+1) table
end
