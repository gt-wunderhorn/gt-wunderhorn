fn=$(basename "$1")
ext="${fn##*.}"

if [ "$ext" == "java" ]; then
  cp $1 Test.java
else
  ../scripts/build_test.sh $1 > Test.java
fi
# sed -i -e 's/\<ArrayList\>/MyList/g' Test.java
# sed -i -e 's/\<List\>/MyList/g' Test.java
# sed -i -e 's/\<LinkedList\>/MyList/g' Test.java
# sed -i -e 's/\<LinkedList\>/MyList/g' Test.java

# sed -i -e 's/\<Arrays\>/MyArrays/g' Test.java
# sed -i -e 's/\<Collections\>/MyArrays/g' Test.java

# sed -i -e 's/import java.util.MyList;/ /g' Test.java
# sed -i -e 's/import java.util.MyArrays;/ /g' Test.java

rm *.class
cp ../benchmark/native/MyNative.java .
# cp ../benchmark/native/MyList.java .
# cp ../benchmark/native/MyArrays.java .
javac -g *.java

./main.byte `cat ../benchmark/classpath` Test $2
