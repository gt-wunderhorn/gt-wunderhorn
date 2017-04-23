#!/usr/bin/env bash
SOURCE_DIR=$(readlink -f "${BASH_SOURCE[0]}")
SOURCE_DIR=$(dirname "$SOURCE_DIR")
source "$SOURCE_DIR/common.sh"

rm -rf bin
mkdir -p bin
cp "$(source_dir)"/../main.byte bin
cd bin

type=$1

function run {
  "$(source_dir)"/run_simple_test.sh "$1" $type
}

echo "Running all simple benchmarks with classpath set to: $(classpath)."

# run "$(source_dir)"/../benchmark/simple/dispatch.fail
# run "$(source_dir)"/../benchmark/simple/dispatch.pass
run "$(source_dir)"/../benchmark/simple/lcm.pass
run "$(source_dir)"/../benchmark/simple/lcm.fail
run "$(source_dir)"/../benchmark/simple/mutableparam.fail
run "$(source_dir)"/../benchmark/simple/slow-add.pass
run "$(source_dir)"/../benchmark/simple/slow-add.fail
run "$(source_dir)"/../benchmark/simple/fib.pass
run "$(source_dir)"/../benchmark/simple/fib.fail
run "$(source_dir)"/../benchmark/simple/mutual_recursion.pass
run "$(source_dir)"/../benchmark/simple/bit_shift.pass
run "$(source_dir)"/../benchmark/simple/bit_shift.fail
run "$(source_dir)"/../benchmark/simple/static_field.fail
run "$(source_dir)"/../benchmark/simple/static_field.pass
run "$(source_dir)"/../benchmark/simple/object_identity.pass
run "$(source_dir)"/../benchmark/simple/object_identity.fail
run "$(source_dir)"/../benchmark/simple/ctor.pass
run "$(source_dir)"/../benchmark/simple/multi_call.pass
run "$(source_dir)"/../benchmark/simple/mixed_array.fail
run "$(source_dir)"/../benchmark/simple/array.fail
run "$(source_dir)"/../benchmark/simple/array.pass
run "$(source_dir)"/../benchmark/simple/method.pass
run "$(source_dir)"/../benchmark/simple/method.fail
run "$(source_dir)"/../benchmark/simple/return_real.fail
run "$(source_dir)"/../benchmark/simple/real_arith.fail
run "$(source_dir)"/../benchmark/simple/real_arith.pass
run "$(source_dir)"/../benchmark/simple/not_null.fail
run "$(source_dir)"/../benchmark/simple/not_null.pass
run "$(source_dir)"/../benchmark/simple/is_null.pass
run "$(source_dir)"/../benchmark/simple/is_null.fail
run "$(source_dir)"/../benchmark/simple/float.pass
run "$(source_dir)"/../benchmark/simple/float.fail
run "$(source_dir)"/../benchmark/simple/non_int_field.fail
run "$(source_dir)"/../benchmark/simple/field.pass
run "$(source_dir)"/../benchmark/simple/field.fail
run "$(source_dir)"/../benchmark/simple/call.pass
run "$(source_dir)"/../benchmark/simple/call.fail
run "$(source_dir)"/../benchmark/simple/while.pass
run "$(source_dir)"/../benchmark/simple/while.fail
