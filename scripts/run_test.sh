cd bin
../scripts/build_test.sh $1 > Test.java
javac Test.java

target=$(basename $1).z3
./main.byte > $target
scheme-format $target > $target.tmp
mv $target.tmp $target
z3 $target
