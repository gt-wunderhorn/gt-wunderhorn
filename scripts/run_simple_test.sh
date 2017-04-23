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
javac -g *.java

echo "Using classpath of $(classpath)."

if [[ $2 = 'run' ]]; then
  ./main.byte "$(classpath)" Test run
  res="$?"
  if [[ "$res" = "$EXPECTED" ]]; then
    echo "Success!"
  else
    echo "Error: Expected $EXPECTED, got $res."
  fi
else
  ./main.byte "$(classpath)" Test "$2"
fi
