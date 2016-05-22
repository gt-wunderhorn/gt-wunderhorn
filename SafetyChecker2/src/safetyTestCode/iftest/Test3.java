package safetyTestCode.iftest;

import infoFlow.ErrorLable;

public class Test3 {

	public void test() {
		int x = 5;
		int y = 0;
		int z = 0;
		if (x == 0)
			z = 1;
		
		if (x != 0)
			z = 1;


		if(x == 0)
			ErrorLable.Error();
	}

}
