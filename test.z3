(set-option :fixedpoint.engine "duality")

; 0 x := 3
; 1 if (x:I != 3) goto 3
; 2 x := 4
; 3 if (x:I == 4) goto 6
; 4 y := 1
; 5 goto 7
; 6 y := 0
; 7 mayinit Assert
; 8 Assert.ensure (y:I)
; 9 return

(declare-rel Line3 (Int))
(declare-rel Line7 (Int))
(declare-rel Goal (Int))

(declare-var x Int)
(declare-var x2 Int)
(declare-var y Int)

(rule (=> (and (= x 3) (not (= x 3))) (Line3 x)))
(rule (=> (and (= x 3) (= x 3) (= x2 3)) (Line3 x2)))
(rule (=> (and (Line3 x) (= x 4) (= y 0)) (Line7 y)))
(rule (=> (and (Line3 x) (not (= x 4)) (= y 1)) (Line7 y)))
(rule (=>
       (not (=> (Line7 y) (= y 1)))
       (Goal y)))

(query Goal)
