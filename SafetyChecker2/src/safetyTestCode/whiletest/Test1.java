package safetyTestCode.whiletest;

import safetyChecker.utilities.ErrorLable;

public class Test1 {

	public void test() {
		int x = 0;
		int y = 50;
		while(x < 3) 
			x++;

		if(x < y)
			if(x+47 == y)
				x = x+40;
//		int y = 0;
//		if(y == 0)
//			x = 0;
//		if(y == 5) 
//			x = 5;

		if(x+7 == y) 
			ErrorLable.Error();
	}

}
