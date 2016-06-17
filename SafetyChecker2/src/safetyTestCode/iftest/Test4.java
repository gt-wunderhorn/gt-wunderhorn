package safetyTestCode.iftest;

import safetyChecker.ErrorLable;

public class Test4 {

	public void test() {
		int x = 5;
		int y = 0;
		int z = 3; 
		if (y != 0)
			z = x;
		else 
			z = 1;

		z = z + 4;

		if(x == z)
			ErrorLable.Error();
	}

}
