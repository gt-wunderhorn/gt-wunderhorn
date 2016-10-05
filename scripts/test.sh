#!/bin/bash

function expect_fail {
  res=$(scripts/run_test.sh $1)
  if [ "$res" != "sat" ]; then
    echo "$1 failed: $res"
    exit 1
  fi
}

function expect_pass {
  res=$(scripts/run_test.sh $1)
  if [ "$res" != "unsat" ]; then
    echo "$1 failed: $res"
    exit 1
  fi
}

expect_pass ../test/multi_pass.test
expect_fail ../test/multi_fail.test
expect_pass ../test/linear_pass.test
expect_fail ../test/linear_fail.test
expect_pass ../test/if_pass.test
expect_fail ../test/if_fail.test
expect_pass ../test/while_pass.test
expect_fail ../test/while_fail.test
