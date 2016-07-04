package safetyTestCode.leetCode.climbingStairs;

import safetyChecker.utilities.ErrorLable;

public class Test1 {

	public static void main(String[] args) {
		Test1 t1 = new Test1();
		int result = t1.climbStairs(4);
		System.out.println(result);
	}

	public int climbStairs(int n) {

		if (n == 1 || n == 0)
			return n;

		int count1 = 1;
		int count2 = 1;

		for (int i = 2; i <= n; i++) {
			int temp = count2;
			count2 = temp + count1;
			count1 = temp;
		}
		
		if(count2 == 8)
			ErrorLable.Error();

		return count2;
	}
}
