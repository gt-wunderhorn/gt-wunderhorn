(** `converge` repeatedly applies a function `f` to an argument `x` until
    the change in `x` caused by `f` is stable. *)
let converge is_stable f x =
  let rec converge' x x' =
    if is_stable x x' then x'
    else converge' x' (f x')
  in
  converge' x (f x)

