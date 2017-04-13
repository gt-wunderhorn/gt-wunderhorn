module JB = Javalib_pack.JBasics
module QID = QualifiedIdentity
module E = Expr

let _ =
  let x = E.Var (Var.Mk (QID.QID ["x"], Type.Int)) in

  Equiv.run "Loop"
            [(JB.TBasic `Int)]
            (JB.TBasic `Int)
            "f"
            "g"
            [x]
            [E.mk_ige x (E.Int 0)]
