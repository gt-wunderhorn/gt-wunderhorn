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

