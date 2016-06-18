package safetyTestCode.leetCode.climbingStairs;

import safetyChecker.ErrorLable;

public class Test2 {

	public static void main(String[] args) {
		Test1 t1 = new Test1();
		int result = t1.climbStairs(4);
		System.out.println(result);
	}

	public int climbStairs(int n) {

		if (n < 0)
			return 0;
		if (n == 1)
			return 1;

		int[] store = new int[n];

		store[0] = 1;
		store[1] = 2;

		for (int i = 2; i < n; ++i)
			store[i] = store[i - 1] + store[i - 2];

		if(store[n-1] == 89)
			ErrorLable.Error();

		return store[n - 1];
	}
}
