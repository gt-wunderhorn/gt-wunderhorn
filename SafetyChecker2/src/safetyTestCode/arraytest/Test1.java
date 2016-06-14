package safetyTestCode.arraytest;

import java.util.Arrays;

import safetyChecker.ErrorLable;

public class Test1 {

	public int test2(int x, int y) {
		int t = 0;
		if(x >= 100)
		       	t = x;
		else 
			t = y;
		return t;
	}

	public void test1() {
		int h = 16;
		int l = 3;
		int o = test2(h,l);
		if(o == h)
			ErrorLable.Error();
	}

	public void test() {

		int x = 3;
		int[] intArray = new int[2];
		int[] intArray2 = new int[2];

		intArray[0] = 25;
		intArray[1] = 20;
		intArray[2] = 15;

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
		intArray2[1] = 20;
		intArray2[2] = 25;
//		System.arraycopy(intArray, 0, intArray2, 0, 3);
//		System.arraycopy(intArray2, 0, intArray, 0, 2);

//		if(Arrays.equals(intArray, intArray2))
		
		if(Arrays.equals(intArray, intArray2))
			ErrorLable.Error();
	}
}
