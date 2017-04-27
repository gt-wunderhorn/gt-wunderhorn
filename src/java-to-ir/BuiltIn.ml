module JB = Javalib_pack.JBasics
module E = Expr

let is_built_in_class cn =
  let name = JB.cn_name cn in
     name = "java.util.Scanner"
  || name = "java.util.Properties"
  || name = "java.util.Arrays"
  || name = "java.util.ArrayList$SubList"
  || name = "java.util.Collections$UnmodifiableList"
  || name = "java.io.BufferedInputStream"
  || name = "java.lang.Boolean"
  || name = "java.lang.Integer"
  || name = "java.lang.Long"
  || name = "java.lang.System"
  || name = "java.lang.Object"
  || name = "java.lang.Class"
  || name = "java.lang.Math"
  || name = "java.lang.Throwable"
  || name = "sun.misc.VM"
  || Algorithm.contains name "String"
  || Algorithm.contains name "Error"
  || Algorithm.contains name "Exception"

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
  ]

let call_built_in_method fn src_line cn ms v args next =

  let name = JB.ms_name ms in
  let s = JB.ms_rtype ms in
  let arbitrary = Instr.Assign (v, E.Var v) in

  if name = "ensure"
  then Some (Instr.Assert (E.mk_eq (List.hd args) (E.Int 1), E.QueryInfo (Assert.User, fn, src_line)))
  else if List.mem name built_in_list
  then Some (match s with
    | None -> Instr.Goto next
    | Some s -> arbitrary)
  else if is_built_in_class cn
  then Some (Instr.Goto next)
  else if name = "valueOf"
  then Some (Instr.Assign (v, List.hd args))
  else None
