package safetyTestCode.iftest;

import infoFlow.ErrorLable;

public class Test3 {

	public void test() {
		int x = 5;
		int y = 0;
		int z = x; 
		if (y == 0)
			z = 1;
		
		if (y != 0)
			z = 5;


		if(x == z)
			ErrorLable.Error();
	}

}
