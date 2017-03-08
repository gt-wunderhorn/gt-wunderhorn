#!/usr/bin/env bash
SOURCE_DIR=$(readlink -f "${BASH_SOURCE[0]}")
SOURCE_DIR=$(dirname "$SOURCE_DIR")
source "$SOURCE_DIR/common.sh"

if [[ $(get_ext "$1") = 'java' ]]; then
    cp "$1" Test.java
else
    build_test "$1" > Test.java
fi

# sed -i -e 's/\<ArrayList\>/MyList/g' Test.java
# sed -i -e 's/\<List\>/MyList/g' Test.java
# sed -i -e 's/\<LinkedList\>/MyList/g' Test.java
# sed -i -e 's/\<LinkedList\>/MyList/g' Test.java

# sed -i -e 's/\<Arrays\>/MyArrays/g' Test.java
# sed -i -e 's/\<Collections\>/MyArrays/g' Test.java

# sed -i -e 's/import java.util.MyList;/ /g' Test.java
# sed -i -e 's/import java.util.MyArrays;/ /g' Test.java

rm -f *.class
cp "$(source_dir)"/../benchmark/native/MyNative.java .
# cp "$(source_dir)"/../benchmark/native/MyList.java .
# cp "$(source_dir)"/../benchmark/native/MyArrays.java .
javac -g *.java

echo "Using classpath of $(classpath)."

./main.byte "$(classpath)" Test $2
