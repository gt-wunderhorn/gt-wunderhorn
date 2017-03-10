#!/usr/bin/env bash
set -euo pipefail

# get the directory the file is in
function source_dir() {
    local SOURCE_DIR=$(readlink -f "${BASH_SOURCE[0]}")
    dirname "$SOURCE_DIR"
}

# find a jar in java's boot classpath
function find_bootcp_jar() {
    java -XshowSettings:properties -version 2>&1 \
        | grep "${1}.jar" \
        | head -1 \
        | awk '{print $NF}'
}

# get the extension of the filename
function get_ext() {
    local fn=$(basename "$1")
    echo "${fn##*.}"
}

# Make a java test class
function build_test() {
    echo 'public class Test {'
    echo '  static void ensure(boolean b) {}'
    sed  's/^/  /' $1
    echo '}'
}

# get the classpath you need to run the source
function classpath() {
    # find rt.jar in the system
    # usually its in /usr/lib/jvm/java-7-openjdk-amd64/jre/lib/rt.jar
    # include the current directory also
    echo "$(pwd):$(find_bootcp_jar rt)"
}

# expect a certain safety outcome
function expect_safety() {
    local OUTPUT=$(eval "${@:2}" | tee /dev/tty)
    local EXPECTED="$1"
    local GOT=$(echo "$OUTPUT" | grep 'User specified' | tail -1 | awk '{print $NF}')
    if [[ $GOT = $EXPECTED ]]; then
        echo "Success!"
        return 0
    else
        echo "Error: Expected $EXPECTED, got $GOT."
        return 64
    fi
}
