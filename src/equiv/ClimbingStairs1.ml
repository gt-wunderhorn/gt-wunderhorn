module JB = Javalib_pack.JBasics
module QID = QualifiedIdentity
module E = Expr

let _ =
  let x = E.Var (Var.Mk (QID.QID ["x"], Type.Int)) in

  Equiv.run "ClimbingStairs1"
            [(JB.TBasic `Int)]
            (JB.TBasic `Int)
            "climbStairs1"
            "climbStairs2"
            [x]
            [E.mk_ige x (E.Int 2)]
