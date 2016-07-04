package safetyTestCode.whiletest;

import safetyChecker.utilities.ErrorLable;

public class Test1 {

	public void test() {
		int x = 0;
		int y = 4;
		while(x < 4) 
			x++;

//		int y = 0;
//		if(y == 0)
//			x = 0;
//		if(y == 5) 
//			x = 5;

		if(x == y) 
			ErrorLable.Error();
	}

}
