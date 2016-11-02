cd bin

fn=$(basename "$1")
ext="${fn##*.}"

if [ "$ext" == "java" ]; then
  cp $1 Test.java
else
  ../scripts/build_test.sh $1 > Test.java
fi
sed -i -e 's/\<ArrayList\>/MyList/g' Test.java
sed -i -e 's/\<List\>/MyList/g' Test.java
sed -i -e 's/\<LinkedList\>/MyList/g' Test.java
sed -i -e 's/\<LinkedList\>/MyList/g' Test.java

sed -i -e 's/\<Arrays\>/MyArrays/g' Test.java
sed -i -e 's/\<Collections\>/MyArrays/g' Test.java

sed -i -e 's/import java.util.MyList;/ /g' Test.java
sed -i -e 's/import java.util.MyArrays;/ /g' Test.java

rm *.class
cp ../native/MyNative.java .
cp ../native/MyList.java .
cp ../native/MyArrays.java .
javac *.java

# target=$(basename $1).z3
./main.byte `cat ../classpath` Test
# > $target
# scheme-format $target > $target.tmp
# mv $target.tmp $target
# z3 $target
