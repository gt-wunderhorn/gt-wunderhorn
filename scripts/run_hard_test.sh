fn=$(basename "$1")
ext="${fn##*.}"

cp $1 $2
cp $3 Test.java

rm *.class
cp ../benchmark/native/MyNative.java .
javac -g *.java

./main.byte `cat ../benchmark/classpath` Test $4
