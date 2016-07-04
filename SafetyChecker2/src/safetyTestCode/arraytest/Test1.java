package safetyTestCode.arraytest;

import java.util.Arrays;

import safetyChecker.utilities.ErrorLable;

public class Test1 {

	public void test4(int n) {
		int[] intArray = new int[2];
		int[] intArray2 = new int[2];

		int i = intArray[0];
		i = intArray2[0];
//		for(int i = 0; i < n; i++)
//			intArray[i] = 3;
				
		if((intArray[0] == 0))  
			ErrorLable.Error();
	}

//	public int test2(int x, int y) {
//		int t = 0;
//		if(x >= 100)
//		       	t = x;
//		else 
//			t = y;
//		return t;
//	}
//
//	public void test1() {
//		int h = 16;
//		int l = 3;
//		int o = test2(h,l);
//		if(o == h)
//			ErrorLable.Error();
//	}
//
//	public void test6(int n){
//		int[][] intArray3 = new int[2][4];
//		intArray3[1][3] = 3;
//		int a = intArray3[0][2];
//
//		ErrorLable.Error();
//
//	}
	
	public void test(int n) {

		int x = 3;
		int[] intArray = new int[3];
		int[] intArray2 = new int[3];

		intArray[0] = 25;
		intArray[1] = 20;
		intArray[2] = 15;

		intArray2[0] = intArray[2];
		intArray2[1] = intArray[1];
		intArray2[2] = intArray[0];
//		intArray[0] = 3;
//		intArray2[1] = 0;

		int[] intArray3 = new int[3];

		intArray3[0] = intArray2[2];
		intArray3[1] = intArray2[1];
		intArray3[2] = intArray2[0];
//		x = intArray2[0];
//		intArray2[0] = intArray[0];
//		intArray2[1] = intArray[1];
//
//		intArray2[0] = 25;
//		intArray2[1] = 20;
//		intArray2[2] = 15;
		System.arraycopy(intArray, 0, intArray2, 0, 3);
//		System.arraycopy(intArray2, 0, intArray, 0, 2);

//		if(Arrays.equals(intArray, intArray2))
		
		if(Arrays.equals(intArray, intArray2))
			ErrorLable.Error();
	}

	public void test3(int n) {
		int[] a1 = new int[n];
		int[] a2 = new int[n];

		for(int i = 0; i < n; i++) {
			a1[i] = i;
			a2[i] = i;
		}	
		a2[n-1] = n+12;
		 
		

		if(Arrays.equals(a1,a2))
			ErrorLable.Error();
	}
}
