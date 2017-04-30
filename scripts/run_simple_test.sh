#!/usr/bin/env bash
SOURCE_DIR=$(readlink -f "${BASH_SOURCE[0]}")
SOURCE_DIR=$(dirname "$SOURCE_DIR")
# shellcheck source=common.sh
source "$SOURCE_DIR/common.sh"

LOGFILE=$(mktemp)

cleanup() {
    rm -rf "$LOGFILE"
}

trap cleanup INT TERM EXIT

ext=$(get_ext "$1")
if [[ $ext = 'java' ]]; then
  cp "$1" Test.java
else
  build_test "$1" > Test.java
fi

rm -f ./*.class
cp "$(source_dir)"/../benchmark/native/MyNative.java .
javac -g ./*.java

if [[ $2 = 'run' ]]; then
  ./main.byte "$(classpath)" Test run &>"$LOGFILE" || res="$?"
  if [[ ($ext = "pass" && "${res:-0}" = "0") || ($ext = "fail" && ("${res:-0}" -ne "0"))]] ; then
    echo "$1: Success, ${res:-0}!"
  else
    echo "$1: Error, ${res:-0}!"
    echo "--- stderr/stdout output of failed test ---"
    cat  "$LOGFILE"
    echo "--- end test output ---"
    exit 1
  fi
else
  ./main.byte "$(classpath)" Test "$2"
fi
