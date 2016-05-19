clear
echo "Starting...>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo ">>> Compiling"
javac -d bin -sourcepath src src/safetyTester/Tester1.java -cp ./bin:./lib/* 
echo ">>> Executing"
java -cp ./bin:./lib/* safetyTester.Tester1
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Ended ..."
