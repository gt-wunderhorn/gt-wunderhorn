#!/usr/bin/env bash
SOURCE_DIR=$(readlink -f "${BASH_SOURCE[0]}")
SOURCE_DIR=$(dirname "$SOURCE_DIR")
source "$SOURCE_DIR/common.sh"

mkdir -p bin
cp "$(source_dir)"/../main.byte bin
cd bin

type=$1

function run {
  echo
  echo $1 $2 $3
  "$(source_dir)"/run_hard_test.sh $1 $2 $3 $type
}

# run ../db/code/snippet_0 MinStack.java ../db/harness/MinStack.harness
# run ../db/code/snippet_1 MinStack.java ../db/harness/MinStack.harness
# run ../db/code/snippet_2 MinStack.java ../db/harness/MinStack.harness
# run ../db/code/snippet_3 MinStack.java ../db/harness/MinStack.harness

cp "$(source_dir)"/../benchmark/hard/harness/TreeNode.java .
run "$(source_dir)"/../benchmark/hard/code/snippet_4 \
    Solution.java \
    "$(source_dir)"/../benchmark/hard/harness/DeleteNode.harness

# run ../benchmark/hard/code/snippet_5 Solution.java ../benchmark/hard/harness/DeleteNode.harness
