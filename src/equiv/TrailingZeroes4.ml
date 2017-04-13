module JB = Javalib_pack.JBasics
module QID = QualifiedIdentity
module E = Expr

let _ =
  let x = E.Var (Var.Mk (QID.QID ["x"], Type.Int)) in

  Equiv.run "TrailingZeroes4"
            [(JB.TBasic `Int)]
            (JB.TBasic `Int)
            "trailingZeroes2"
            "trailingZeroes4"
            [x]
            [E.mk_ige x (E.Int 5)]
