package safetyTestCode.iftest;

import infoFlow.ErrorLable;

public class Test2 {

	public void test() {
		int x = 5;
		int y = 0;
		int z = 5;

		if (y == 0) {
			x = x - 2;
			z = 3;
		}

		if(x != z)
			ErrorLable.Error();
	}

}
