module JB = Javalib_pack.JBasics
module QID = QualifiedIdentity
module E = Expr

let _ =
  let x = E.Var (Var.Mk (QID.QID ["x"], Type.Int)) in

  Equiv.run "TrailingZeroes3"
            [(JB.TBasic `Int)]
            (JB.TBasic `Int)
            "trailingZeroes1"
            "trailingZeroes3"
            [x]
            [E.mk_ige x (E.Int 5)]
