package safetyTestCode.arraytest;

import infoFlow.ErrorLable;

public class Test1 {

	public void test() {

		int x = 3;
		int[] intArray = new int[2];
		intArray[0] = 10;
		intArray[1] = 20;

		int[] intArray2 = new int[2];
		intArray2[0] = intArray[0];
		intArray2[1] = intArray[1];
		intArray[0] = 3;

		x = intArray2[0];

		if(!intArray2.equals(intArray))
			ErrorLable.Error();
	}

	public void test2(int x, int y) {
		int t = 0;
		if(x > y) t = x;
		else t = y;

		if(t < x) ErrorLable.Error();
	}
}
