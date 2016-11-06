let no_special _ = None

let specialize base special x =
  match special x with
  | Some y -> y
  | None -> base x
