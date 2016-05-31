package safetyTestCode.whiletest;

import infoFlow.ErrorLable;

public class Test1 {

	public void test() {
		int x = 5;
		int y = 1;

		while(x < 9) {
			x = x + y;
		}

//		int y = 0;
//		if(y == 0)
//			x = 0;
//		if(y == 5) 
//			x = 5;

		if(x != y)
			ErrorLable.Error();
	}

}
