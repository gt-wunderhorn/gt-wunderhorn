#!/bin/bash

mkdir -p bin
cp main.byte bin
cd bin

type=$1

function run {
  echo
  echo $1 $2 $3
  ../scripts/run_hard_test.sh $1 $2 $3 $type
}

# run ../db/code/snippet_0 MinStack.java ../db/harness/MinStack.harness
# run ../db/code/snippet_1 MinStack.java ../db/harness/MinStack.harness
# run ../db/code/snippet_2 MinStack.java ../db/harness/MinStack.harness
# run ../db/code/snippet_3 MinStack.java ../db/harness/MinStack.harness

cp ../benchmark/hard/harness/TreeNode.java .
run ../benchmark/hard/code/snippet_4 Solution.java ../benchmark/hard/harness/DeleteNode.harness
# run ../benchmark/hard/code/snippet_5 Solution.java ../benchmark/hard/harness/DeleteNode.harness
