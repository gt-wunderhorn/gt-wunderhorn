(set-option :fixedpoint.engine "duality")
(declare-var ID_0 Int)
(declare-var ID_1 Int)
(declare-var ID_2 Int)
(declare-var ret_0 Int)
(declare-var ret_1 Int)
(declare-var x_0 Int)
(declare-var x_1 Int)
(declare-var y_0 Int)
(declare-var y_1 Int)
(declare-var v_p1_2_0 Int)
(declare-var v_p1_2_1 Int)
(declare-rel r_p1_4 (Int Int))
(declare-rel r_p1_8 (Int))
(declare-rel r_p1_RET
             ((Array Int Int) Int Int Int Int Int Int Int Int Int Int))
(declare-rel p2 (Int Int Int))
(declare-rel q (Bool))

(rule (=> (and (= ID_1 1)
               (= ID_2 (+ ID_1 1))
               (= x_1 ID_2))
          (p2 ID_2 ret_0 x_1)))

(rule
 (=> (and (p2 ID_0 ret_0 x_0)
          (= x_1 ret_0)
          (= ID_1 (+ ID_0 1))
          (= y_1 ID_1))
     (p2 ID_1 ret_0 x_1)))

(rule
 (=> (and (p2 ID_0 ret_0 x_0)
          (= y_1 ret_0))
     (r_p1_4 x_0 y_1)))

(rule (=> (and (r_p1_4 x_0 y_0)
               (not (= x_0 y_0)))
          (r_p1_8 1)))

(rule (=> (and (r_p1_4 x_0 y_0)
               (= x_0 y_0))
          (r_p1_8 0)))

(rule (=> (not (=> (r_p1_8 v_p1_2_0) (= v_p1_2_0 1))) (q (= v_p1_2_0 1))))
(query q)
