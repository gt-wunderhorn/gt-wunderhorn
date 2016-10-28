module L = Lang
module V_set = L.V_set
module G = L.PG

let x = ("x", L.Int)
let y = ("y", L.Int)
let z = ("z", L.Int)

let test_path_uses_before_assigns =
  let is =
    [ L.Assign (x, L.Var x)
    ; L.Assign (y, L.Int_lit 0)
    ; L.Assign (y, L.mk_add (L.Var z) (L.Var y))
    ] in

  let expected = V_set.of_list [ x ; z ] in
  let actual = L.path_uses_before_assigns is in
  V_set.diff expected actual = V_set.empty

let tests =
  [ "path uses before assigns", test_path_uses_before_assigns
  ]
