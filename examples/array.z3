; 1 x := new int [2]
; 2 x[0] := 5
; 3 if (x[0] != 5) goto 5
; 4 x[1] := 2
; 5 if (x[1] != 2) goto 8
; 6 q := 1
; 7 goto 9
; 8 q := 0
; 9 Test.ensure (q:I)
; 10 return
(set-option :fixedpoint.engine "duality")

(declare-var x1 (Array Int Int))
(declare-var x2 (Array Int Int))
(declare-var q Int)

(declare-rel Line3 ((Array Int Int)))
(declare-rel Line5 ((Array Int Int)))
(declare-rel Line9 (Int))
(declare-rel Goal (Int))

(rule (=> (= x2 (store x1 0 5)) (Line3 x2)))

(rule (=> (and (Line3 x1) (= (select x1 0) 5) (= x2 (store x1 1 2))) (Line5 x2)))
(rule (=> (and (Line3 x1) (not (= (select x1 0) 5))) (Line5 x2)))

(rule (=> (and (Line5 x1) (= (select x1 1) 2) (= q 1)) (Line9 q)))
(rule (=> (and (Line5 x1) (not (= (select x1 1) 2)) (= q 0)) (Line9 q)))

(rule (=> (not (=> (Line9 q) (= q 1))) (Goal q)))

(query Goal)
