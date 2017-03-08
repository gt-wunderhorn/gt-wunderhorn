#!/bin/bash

mkdir -p bin
cp main.byte bin
cd bin

type=$1

function run {
  echo
  echo $1
  ../scripts/run_simple_test.sh $1 $type
}

run ../benchmark/simple/fib.pass
run ../benchmark/simple/fib.fail
run ../benchmark/simple/mutual_recursion.pass
run ../benchmark/simple/bit_shift.pass
run ../benchmark/simple/bit_shift.fail
run ../benchmark/simple/static_field.fail
run ../benchmark/simple/static_field.pass
run ../benchmark/simple/object_identity.pass
run ../benchmark/simple/object_identity.fail
run ../benchmark/simple/ctor.pass
run ../benchmark/simple/dispatch.pass
run ../benchmark/simple/multi_call.pass
run ../benchmark/simple/mixed_array.fail
run ../benchmark/simple/array.fail
run ../benchmark/simple/array.pass
run ../benchmark/simple/method.pass
run ../benchmark/simple/method.fail
run ../benchmark/simple/return_real.fail
run ../benchmark/simple/real_arith.fail
run ../benchmark/simple/real_arith.pass
run ../benchmark/simple/not_null.fail
run ../benchmark/simple/not_null.pass
run ../benchmark/simple/is_null.pass
run ../benchmark/simple/is_null.fail
run ../benchmark/simple/float.pass
run ../benchmark/simple/float.fail
run ../benchmark/simple/non_int_field.fail
run ../benchmark/simple/field.pass
run ../benchmark/simple/field.fail
run ../benchmark/simple/call.pass
run ../benchmark/simple/call.fail
run ../benchmark/simple/while.pass
run ../benchmark/simple/while.fail
