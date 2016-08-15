package safetyTestCode.interproc;

import safetyChecker.utilities.ConsraintAnnotation;
import safetyChecker.utilities.ErrorLable;

public class Test1 {

	int j;
	@ConsraintAnnotation(constraint=true)
	public void test(int i) {
		int j = recursive(3);
	//	int j = getHalf(10);
	//	int j = fun1(1);
		if(j == 2) 
			ErrorLable.Error();	
	}

	public int recursive(int i) {
		if(i == 1 || i == 2)
			return 1;
		else
			return recursive(i-1) 
				+ recursive(i-2);
	}
	
	public int recursive2(int i) {
		int result = 0;
		if(i == 1)
			result = 1;
		else
			result = recursive(i-1) + i;
		return result;
	}

	public int getHalf(int i) {
		boolean aa = isEven(i);
		int result = 0;
		if(aa)
			result = i/2;
		else
			result = (i+1)/2;

		return result;
	}

	public boolean isEven(int n) {
		boolean result = false;
		if(n % 2 == 0)
			result = true;
		else 
			result = false;

		return result;

	}

	public int fun1(int in) {
		int result = 0;
		int temp = fun2(in);
		if(in == 2)
			result = in * temp;
		else 
			result = temp;
		return result;
	}

	public int fun2(int i) {
		return 50;
	}


	public static void main(String[] args) {
	
		Test1 test1 = new Test1();
		int j = test1.recursive(3);
		System.out.println(j);

	}

	public void test4(int k) {
		int j = 0;
		int i = Test1.getOne(); 
		if(i == 5)
			ErrorLable.Error();
	}

	public static int getOne() {
		return 5; 
	}


	public static int staticSum(int i, int j) {
		return i + j;
	}

	public int sum(int i, int j) {
		return i + j;
	}

	public void test3(int n) {
		int j = 1;
		int k = 5;
		Obj obj = new Obj();
		j = obj.getSix();
		int i = getNumber(j, k, obj);
		if(i == 211) 
			ErrorLable.Error();

	}

	public int getNumber(int j, int k, Obj obj){
			
		int i = j + obj.getDouble(k);
		return i;
	}



	int getDouble(int k) { return k*2; }


	public void test2(int n) {

		int i = fibonacciRecusion(n);
		if(i == 2)
			ErrorLable.Error();
	}

	public int fibonacciRecusion(int number){
		if(number == 1 || number == 2){
			return 1;
		}
		return fibonacciRecusion(number-1) + fibonacciRecusion(number -2); //tail recursion
	 }


}



