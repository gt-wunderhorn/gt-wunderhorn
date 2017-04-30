#!/usr/bin/env bash
SOURCE_DIR=$(readlink -f "${BASH_SOURCE[0]}")
SOURCE_DIR=$(dirname "$SOURCE_DIR")
# shellcheck source=common.sh
source "$SOURCE_DIR/common.sh"

cp "$1" "$2"
cp "$3" Test.java

rm -f ./*.class
cp "$(source_dir)"/../benchmark/native/MyNative.java .
javac -g ./*.java

echo "Using classpath of $(classpath)."

./main.byte "$(classpath)" Test "$4"
