module JB = Javalib_pack.JBasics
module E = Expr
module PG = Program_graph

let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

let is_built_in_class cn =
  let name = JB.cn_name cn in
  name = "java.util.Scanner" ||
  name = "java.util.Properties" ||
  name = "java.util.Arrays" ||
  name = "java.util.ArrayList$SubList" ||
  name = "java.util.Collections$UnmodifiableList" ||
  name = "java.io.BufferedInputStream" ||
  name = "java.lang.Boolean" ||
  name = "java.lang.Integer" ||
  name = "java.lang.Long" ||
  name = "java.lang.System" ||
  name = "java.lang.Object" ||
  name = "java.lang.Class" ||
  name = "java.lang.Math" ||
  name = "java.lang.Throwable" ||
  name = "sun.misc.VM" ||
  contains name "String" ||
  contains name "Error" ||
  contains name "Exception"

let call_built_in_method cn ms v args next =
  let built_in_list =
    [ "hasNextShort"
    ; "hasNextInt"
    ; "hasNextLong"
    ; "hasNextBigInteger"
    ; "hasNextFloat"
    ; "hasNextDouble"
    ; "hasNextBoolean"
    ; "nextBoolean"
    ; "nextShort"
    ; "nextInt"
    ; "nextLong"
    ; "nextBigInteger"
    ; "nextFloat"
    ; "nextDouble"
    ; "print"
    ; "println"
    ; "close"
    ; "flush"
    ; "getClass"
    ; "getComponentType"
    ; "desiredAssertionStatus"
    ; "getPrimitiveClass"
    ; "getSavedProperty"
    ; "outOfBoundsMsg"
    ; "floatToRawIntBits"
    ; "doubleToRawLongBits"
    ; "toString"
    ; "stringSize"
    ; "getChars"
    ; "checkForComodification"
    ; "newArray"
    ; "hugeCapacity"
    ; "copyOf"
    ] in

  let name = JB.ms_name ms in
  let s = JB.ms_rtype ms in
  let arbitrary = Ir.Assign (v, E.Var v) in

  if name = "ensure"
  then Some (Ir.Assert (E.mk_eq (List.hd args) (E.Int_lit 1), PG.User))
  else if List.mem name built_in_list
  then Some (match s with
    | None -> Ir.Goto next
    | Some s -> arbitrary)
  else if is_built_in_class cn
  then Some (Ir.Goto next)
  else if name = "valueOf"
  then Some (Ir.Assign (v, List.hd args))
  else None
