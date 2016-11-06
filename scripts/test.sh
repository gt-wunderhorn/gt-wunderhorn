#!/bin/bash

function run {
  echo
  echo $1
  scripts/run_test.sh $1
}

# run ../test/MyListTest.java
# run ../test/ArrayList.java
# run ../test/Birthday.java
run ../test/bit_shift.pass
run ../test/bit_shift.fail
run ../test/static_field.fail
run ../test/static_field.pass
run ../test/object_identity.pass
run ../test/object_identity.fail
run ../test/ctor.pass
run ../test/not_null.pass
run ../test/dispatch.pass
run ../test/multi_call.pass
run ../test/mixed_array.fail
run ../test/array.pass
run ../test/lcm.pass
run ../test/lcm.fail
run ../test/method.pass
run ../test/method.fail
run ../test/return_real.fail
run ../test/real_arith.fail
run ../test/real_arith.pass
run ../test/not_null.fail
run ../test/is_null.pass
run ../test/is_null.fail
run ../test/float.pass
run ../test/float.fail
run ../test/div.pass
run ../test/div.fail
run ../test/non_int_field.fail
run ../test/field.pass
run ../test/field.fail
run ../test/call.pass
run ../test/call.fail
run ../test/while.pass
run ../test/while.fail
