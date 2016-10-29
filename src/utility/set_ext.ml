module Make(C : Comparable.T) = struct
  module Underlying = Set.Make(C)
  include Underlying

  let unions = List.fold_left union empty
  let unions_map f xs = unions (List.map f xs)

  let of_list =
    List.fold_left (fun m x -> add x m) empty

  (** Apply a function f to each item in the set.
      **WARNING** The output set may not have the same size as the input set.
      Specifically, if `f` can generate the same output for multiple inputs,
      the size of the set might go down. *)
  let map f s =
    s |> elements |> List.map f |> of_list
end
