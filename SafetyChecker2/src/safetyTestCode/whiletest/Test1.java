package safetyTestCode.whiletest;

import safetyChecker.ErrorLable;

public class Test1 {

	public void test() {
		int x = 5;
		int y = 1;

		while(y != 5) {
			x = x + y;
			y = y+2;
		}

//		int y = 0;
//		if(y == 0)
//			x = 0;
//		if(y == 5) 
//			x = 5;

		if(x != y+4)
			ErrorLable.Error();
	}

}
