module JB = Javalib_pack.JBasics
module QID = QualifiedIdentity
module E = Expr

let _ =
  let x = E.Var (Var.Mk (QID.QID ["x"], Type.Int)) in

  Equiv.run "AddDigits2"
            [(JB.TBasic `Int)]
            (JB.TBasic `Int)
            "addDigits1"
            "addDigits3"
            [x]
            [E.mk_igt x (E.Int 0)]
