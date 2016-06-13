package safetyTestCode.arraytest;

import java.util.Arrays;

import infoFlow.ErrorLable;

public class Test1 {

	public void test2(int x, int y) {
		int t = 0;
		if(x > y) t = x;
		else t = y;

		if(t < x) ErrorLable.Error();
	}

	public void test() {

		int x = 3;
		int[] intArray = new int[2];
		int[] intArray2 = new int[1];

		intArray[0] = 15;
//		intArray[1] = 20;
//		intArray[2] = 25;

//		int[] intArray2 = new int[2];
//		intArray2[0] = intArray[0];
//		intArray2[1] = intArray[1];
//		intArray[0] = 3;
//		intArray2[1] = 0;


//		x = intArray2[0];
//		intArray2[0] = intArray[0];
//		intArray2[1] = intArray[1];
//
		intArray2[0] = 15;
//		intArray2[1] = 20;
//		intArray2[2] = 25;
//		System.arraycopy(intArray, 0, intArray2, 0, 3);
//		System.arraycopy(intArray2, 0, intArray, 0, 2);

//		if(Arrays.equals(intArray, intArray2))
		
		if(Arrays.equals(intArray, intArray2))
			ErrorLable.Error();
	}
}
