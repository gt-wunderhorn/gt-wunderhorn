#!/usr/bin/env bash
SOURCE_DIR=$(readlink -f "${BASH_SOURCE[0]}")
SOURCE_DIR=$(dirname "$SOURCE_DIR")
source "$SOURCE_DIR/common.sh"

PROGRAM="$(readlink -f "$1")"
mkdir -p bin
cp "$(source_dir)"/../main.byte bin
cp "$(source_dir)"/../SawjaInspect.byte bin
cd bin

echo
echo "Generating Z3 SMT for $1"
"$(source_dir)"/run_simple_test.sh "${PROGRAM}" print

echo
echo "Generating JBir representation for $1"
mkdir -p jbir-html
CLASSPATH="$(classpath)" ./SawjaInspect.byte
