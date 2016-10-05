module Make(C : Comparable.T) = struct
  module Underlying = Set.Make(C)
  include Underlying

  let unions = List.fold_left union empty
  let unions_map f xs = unions (List.map f xs)
end
