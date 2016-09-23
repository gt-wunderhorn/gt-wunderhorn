open Abstr_lang

let program =
  let x = Variable "x" in
  let y = Variable "y" in
  [ Assign (x, (Int_lit 3))                       (* 0 *)
  ; If (Not (Eq (Var x, Int_lit 3)), Line 3)      (* 1 *)
  ; Assign (x, (Int_lit 4))                       (* 2 *)
  ; If (Eq (Var x, Int_lit 4), Line 6)            (* 3 *)
  ; Assign (y, (Int_lit 1))                       (* 4 *)
  ; Goto (Line 7)                                 (* 5 *)
  ; Assign (y, (Int_lit 0))                       (* 6 *)
  ; Assert y                                      (* 7 *)
  ]

