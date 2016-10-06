$bcvar1 := new int [2][2]
$bcvar1:java.lang.Object[0]:java.lang.Object[0] := 1
if ($bcvar1:java.lang.Object[0]:java.lang.Object[0]:I != 1) goto 18
$bcvar1:java.lang.Object[1]:java.lang.Object[1] := 5
if ($bcvar1:java.lang.Object[1]:java.lang.Object[1]:I != 5) goto 25
$T0_42 := 1
goto 26
$T0_42 := 0
mayinit Test
Test.ensure ($T0_42:I)
return



(set-option :fixedpoint.engine "duality")

(declare-var arr (Array Int (Array Int (Array Int Int))))
(declare-var arr2 (Array Int (Array Int (Array Int Int))))
(declare-var xs Int)

(declare-rel p (Int (Array Int (Array Int (Array Int Int)))))
(declare-rel Goal (Int))

(rule (=> (= arr2 (store arr xs
                    (store (select arr xs) 0
                      (store (select (select arr xs) 0)
                        1 2))))
          (p xs arr2)))

(rule (=> (not (=> (p xs arr)
                   (= (select (select (select arr xs) 0) 1) 2)))
          (Goal xs)))

(query Goal)
