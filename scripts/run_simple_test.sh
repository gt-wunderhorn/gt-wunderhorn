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

rm -f *.class
cp "$(source_dir)"/../benchmark/native/MyNative.java .
javac -g *.java

if [[ $2 = 'run' ]]; then
  ./main.byte "$(classpath)" Test run &>/dev/null
  res="$?"
  if [[ "$res" = "$EXPECTED" ]]; then
    echo "$1: Success!"
  else
    echo "$1: Error, expected $EXPECTED, got $res."
  fi
else
  ./main.byte "$(classpath)" Test "$2"
fi
