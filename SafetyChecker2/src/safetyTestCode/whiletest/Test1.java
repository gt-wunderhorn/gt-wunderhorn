package safetyTestCode.whiletest;

import safetyChecker.ErrorLable;

public class Test1 {

	public void test() {
		int x = 0;

		while(x < 2) 
			x++;

//		int y = 0;
//		if(y == 0)
//			x = 0;
//		if(y == 5) 
//			x = 5;

		if(x == 2)
			ErrorLable.Error();
	}

}
