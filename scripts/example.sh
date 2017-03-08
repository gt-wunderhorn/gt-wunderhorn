#!/bin/bash

mkdir -p bin
cp main.byte bin
cd bin
../scripts/run_simple_test.sh ../benchmark/simple/while.pass print
scheme-format example.z3 > temp
mv temp example.z3
