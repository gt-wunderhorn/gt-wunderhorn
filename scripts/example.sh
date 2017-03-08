#!/usr/bin/env bash
SOURCE_DIR=$(readlink -f "${BASH_SOURCE[0]}")
SOURCE_DIR=$(dirname "$SOURCE_DIR")
source "$SOURCE_DIR/common.sh"

PROGRAM="$(readlink -f "$1")"
mkdir -p bin
cp "$(source_dir)"/../main.byte bin
cd bin

"$(source_dir)"/run_simple_test.sh "${PROGRAM}" print
