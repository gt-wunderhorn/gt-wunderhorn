package safetyTestCode;

import infoFlow.ErrorLable;

public class Test1 {

	public void test() {
		int x = 5;
//		int y = 0;
//		int z = 0;
//		if (y == 0)
//			z = 1;
//		else 
//			z = 0;

		if(x == 5)
			ErrorLable.Error();
	}

}
