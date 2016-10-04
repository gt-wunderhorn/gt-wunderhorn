#!/bin/bash

function make_test {
  echo 'public class Test {'
  echo '  static void ensure(boolean b) {}'
  sed  's/^/  /' $1
  echo '}'
}

make_test $1 > Test.java
javac Test.java

./main.byte > test.z3
z3 test.z3

rm Test.java
rm test.z3
rm Test.class
