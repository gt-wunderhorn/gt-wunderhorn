cd bin
../scripts/build_test.sh $1 > Test.java
javac Test.java
./main.byte > test.z3
z3 test.z3

# ../scripts/clean.sh
