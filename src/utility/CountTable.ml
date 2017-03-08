(** A count table keeps a counter for each element in the domain. A new table
    can be created with `mk`, passing in the domain. Each element in the domain
    will start with a count of 0. This count can be incremented with `increment`
    and accessed with `get`. *)

module Make (C : Comparable.T) = struct
  module Underlying = Map.Make(C)

  type t = (int Underlying.t) ref

  let empty () : t = ref Underlying.empty

  let get table index =
    if Underlying.mem index !table
    then ()
    else table := Underlying.add index 0 !table;
    Underlying.find index !table

  let increment table index =
    table := Underlying.add index ((get table index)+1) !table
end
