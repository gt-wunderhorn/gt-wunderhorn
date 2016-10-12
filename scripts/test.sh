#!/bin/bash

function run {
  res=$(scripts/run_test.sh $1 | tail -1)
  fn=$(basename "$1")
  ext="${fn##*.}"


  if [ "$ext" == "pass" ]; then
    if [ "$res" != "unsat" ]; then
      echo "$1 failed: $res"
      exit 1
    fi
  elif [ "$ext" == "fail" ]; then
    if [ "$res" != "sat" ]; then
      echo "$1 failed: $res"
      exit 1
    fi
  else
    echo "invalid extension: $ext"
    exit 1
  fi
}

# expect_pass ../test/nested.pass

run ../test/method.pass
run ../test/method.fail
run ../test/return_real.fail
run ../test/real_arith.fail
run ../test/real_arith.pass
run ../test/not_null.pass
run ../test/not_null.fail
run ../test/is_null.pass
run ../test/is_null.fail
run ../test/float.pass
run ../test/float.fail
run ../test/div.pass
run ../test/div.fail
run ../test/object_identity.pass
run ../test/field.pass
run ../test/field.fail
run ../test/array.pass
run ../test/array.fail
run ../test/multi_call.pass
run ../test/call.pass
run ../test/call.fail
run ../test/while.pass
run ../test/while.fail
run ../test/if.pass
run ../test/if.fail
run ../test/linear.pass
run ../test/linear.fail
