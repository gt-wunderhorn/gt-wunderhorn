package safetyTestCode.iftest;

import safetyChecker.ErrorLable;

public class Test1 {

	public void test() {
		int x = callee();
		if(x != 3)
			ErrorLable.Error();
	}

	public int callee() {

		return 3;
//		int x = 5;
//		int y = 3;
//		x = y+1;
//		x = x-1;
//		return x;
	}

}
