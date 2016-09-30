open Program

let program =
  let x = Variable "x" in
  let y = Variable "y" in
  [ Linear (Assign (x, (Int_lit 3)))                       (* 0 *)
  ; Non_linear (If (Eq (Var x, Int_lit 3), 3))             (* 1 *)
  ; Linear (Assign (x, (Int_lit 4)))                       (* 2 *)
  ; Non_linear (If (Eq (Var x, Int_lit 4), 6))             (* 3 *)
  ; Linear (Assign (y, (Int_lit 1)))                       (* 4 *)
  ; Non_linear (Goto 7)                                    (* 5 *)
  ; Linear (Assign (y, (Int_lit 0)))                       (* 6 *)
  ; Linear (Assert y)                                      (* 7 *)


  ; Linear (Assign (x, (Int_lit 5)))                       (* 8 *)
  ; Non_linear (If (Eq (Var x, Int_lit 5), 11))            (* 9 *)
  ; Linear (Assign (x, (Int_lit 6)))                       (* 10 *)
  ; Non_linear (If (Eq (Var x, Int_lit 6), 14))            (* 11 *)
  ; Linear (Assign (y, (Int_lit 1)))                       (* 12 *)
  ; Non_linear (Goto 15)                                   (* 13 *)
  ; Linear (Assign (y, (Int_lit 0)))                       (* 14 *)
  ; Linear (Assert y)                                      (* 15 *)
  ]

let _ =
  Printf.printf "%s\n" (Inspect.inspect program)
