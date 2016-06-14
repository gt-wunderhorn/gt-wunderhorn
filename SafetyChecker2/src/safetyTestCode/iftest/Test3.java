package safetyTestCode.iftest;

import safetyChecker.ErrorLable;

public class Test3 {

	public void test() {
		int x = 5;
//		int y = 0;
//		int z = x; 
		if (x < 6)
			x = x + 1;
		

		if (x < 6)
			x = x + 1;

//		x = z ;
//		z = z + 4;

		if(x != 6)
			ErrorLable.Error();
	}

}
