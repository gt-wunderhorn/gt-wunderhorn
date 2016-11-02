cd bin

fn=$(basename "$1")
ext="${fn##*.}"

if [ "$ext" == "java" ]; then
  cp $1 Test.java
else
  ../scripts/build_test.sh $1 > Test.java
fi

rm *.class
cp ../test/MyNative.java .
cp ../test/MyList.java .
javac *.java

# target=$(basename $1).z3
./main.byte `cat ../classpath` Test
# > $target
# scheme-format $target > $target.tmp
# mv $target.tmp $target
# z3 $target
