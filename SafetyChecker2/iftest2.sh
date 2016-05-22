clear
echo "Starting...>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo ">>> clean .dot and .ps files"
rm *ps
rm *dot
echo ">>> Compiling"
javac -d bin -sourcepath src src/safetyTester/iftest/Tester2.java -cp ./bin:./lib/* 
echo ">>> Executing"
java -cp ./bin:./lib/* safetyTester.iftest.Tester2
echo ">>> creating .ps files"
python PngCreator.py
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Ended ..."
