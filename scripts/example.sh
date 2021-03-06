#!/usr/bin/env bash
SOURCE_DIR=$(readlink -f "${BASH_SOURCE[0]}")
SOURCE_DIR=$(dirname "$SOURCE_DIR")
# shellcheck source=common.sh
source "$SOURCE_DIR/common.sh"

PROGRAM="$(readlink -f "$1")"
mkdir -p bin
cp -r "$(source_dir)"/../main.byte bin
cp -r "$(source_dir)"/../SawjaInspect.byte bin
cd bin || exit 1

echo
echo "Generating Z3 SMT for $1"
"$(source_dir)"/run_simple_test.sh "${PROGRAM}" print

echo
if hash scheme-format 2>/dev/null; then
    echo "Formatting Z3 output"
    scheme-format -i 'example.z3'
else
    echo "scheme-format not found, not formatting Z3."
fi

echo
echo "Generating JBir representation for $1"
mkdir -p jbir-html
CLASSPATH="$(classpath)" ./SawjaInspect.byte

echo
if hash z3 2>/dev/null; then
    echo "Testing program safety (unsat is safe, sat is unsafe)..."
    z3 'example.z3'
else
    echo "z3 not found, will not test safety."
fi

