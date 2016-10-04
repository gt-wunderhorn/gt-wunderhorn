#!/bin/bash

./make_test $1 > Test.java
javac Test.java

./example.byte > test.z3
z3 test.z3

# rm Test.java
# rm test.z3
# rm Test.class
