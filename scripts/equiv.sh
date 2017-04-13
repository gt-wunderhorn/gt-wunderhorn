cp benchmark/equiv/$1.java bin

echo $1

mkdir -p bin
cp -r $1.byte bin
cd bin

javac -g $1.java

./$1.byte `cat ../benchmark/classpath` run
