(set-option :fixedpoint.engine "duality")

(declare-var Pair_x_0 (Array Int Int))
(declare-var Pair_x_1 (Array Int Int))
(declare-var Pair_y_0 (Array Int Int))
(declare-var Pair_y_1 (Array Int Int))
(declare-var p (Int))

(declare-rel If (Int (Array Int Int)))
(declare-rel Assertion (Int (Array Int Int)))
(declare-rel Goal (Int))

(rule (=> (and (= p 0)
               (= Pair_x_1 (store Pair_x_0 p 0)))
          (If p Pair_x_1)))

(rule (=> (and (If p Pair_x_0)
               (= (select Pair_x_0 p) 0)
               (= Pair_y_1 (store Pair_y_0 p 5)))
          (Assertion p Pair_y_1)))

(rule (=> (and (If p Pair_x_0)
               (not (= (select Pair_x_0 p) 0)))
          (Assertion p Pair_y_0)))

(rule (=> (not (=> (Assertion p Pair_y_0)
                   (= (select Pair_y_0 p) 5)))
          (Goal p)))

(query Goal)
