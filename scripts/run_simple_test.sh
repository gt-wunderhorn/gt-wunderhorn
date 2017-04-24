#!/usr/bin/env bash
SOURCE_DIR=$(readlink -f "${BASH_SOURCE[0]}")
SOURCE_DIR=$(dirname "$SOURCE_DIR")
source "$SOURCE_DIR/common.sh"

if [[ $(get_ext "$1") = 'java' ]]; then
  cp "$1" Test.java
else
  build_test "$1" > Test.java
fi

if [[ $(get_ext "$1") = 'pass' ]]; then
  EXPECTED="0"
elif [[ $(get_ext "$1") = 'fail' ]]; then
  EXPECTED="1"
fi

ext=$(get_ext "$1")

rm -f *.class
cp "$(source_dir)"/../benchmark/native/MyNative.java .
javac -g *.java

if [[ $2 = 'run' ]]; then
  ./main.byte "$(classpath)" Test run &>/dev/null || res="$?"
  if [[ ($ext = "pass" && "${res:-0}" = "0") || ($ext = "fail" && ("${res:-0}" -ne "0"))]] ; then
    echo "$1: Success, ${res:-0}!"
  else
    echo "$1: Error, ${res:-0}!"
    exit 1
  fi
else
  ./main.byte "$(classpath)" Test "$2"
fi
