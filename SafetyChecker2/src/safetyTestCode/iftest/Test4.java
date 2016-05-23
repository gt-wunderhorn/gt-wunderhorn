package safetyTestCode.iftest;

import infoFlow.ErrorLable;

public class Test4 {

	public void test() {
		int x = 5;
		int y = 0;
		int z = 3; 
		if (y == 0)
			z = x;
		else 
			z = 1;

		x = 3;

		if(x == z)
			ErrorLable.Error();
	}

}
