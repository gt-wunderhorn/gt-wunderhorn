module L = Lang

let remove_simple_assignments_test =
  let x = L.Var ("x", L.Int) in
  let y = L.Var ("y", L.Int) in
  let z = L.Var ("z", L.Int) in
  let one = L.Int_lit 1 in
  let original =
    L.mk_impl
      (L.mk_and
         [ L.Relation (0, [x])
         ; L.mk_eq x one
         ; L.mk_eq y x
         ; L.mk_eq z y
         ])
      (L.Relation (1, [z]))
  in

  let expected =
    L.mk_impl
      (L.Relation (0, [one]))
      (L.Relation (1, [one]))
  in

  let actual = Simplify.remove_simple_assignments original in
  actual = expected
