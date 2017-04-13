module JB = Javalib_pack.JBasics
module QID = QualifiedIdentity
module E = Expr

let _ =
  let x = E.Var (Var.Mk (QID.QID ["x"], Type.Int)) in

  Equiv.run "ReverseInteger1"
            [(JB.TBasic `Int)]
            (JB.TBasic `Int)
            "reverse1"
            "reverse2"
            [x]
            [E.mk_ige x (E.Int 0)]
