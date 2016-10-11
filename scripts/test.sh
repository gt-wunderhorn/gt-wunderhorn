#!/bin/bash

function expect_fail {
  res=$(scripts/run_test.sh $1 | tail -1)
  if [ "$res" != "sat" ]; then
    echo "$1 failed: $res"
    exit 1
  fi
}

function expect_pass {
  res=$(scripts/run_test.sh $1 | tail -1)
  if [ "$res" != "unsat" ]; then
    echo "$1 failed: $res"
    exit 1
  fi
}

# expect_pass ../test/method.test
# expect_pass ../test/nested.test

expect_pass ../test/object_identity.test
expect_pass ../test/field_pass.test
expect_fail ../test/field_fail.test
expect_pass ../test/array_pass.test
expect_fail ../test/array_fail.test
expect_pass ../test/multi_call_pass.test
expect_pass ../test/call_pass.test
expect_fail ../test/call_fail.test
expect_pass ../test/while_pass.test
expect_fail ../test/while_fail.test
expect_pass ../test/if_pass.test
expect_fail ../test/if_fail.test
expect_pass ../test/linear_pass.test
expect_fail ../test/linear_fail.test
