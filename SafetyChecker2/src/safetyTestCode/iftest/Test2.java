package safetyTestCode.iftest;

import infoFlow.ErrorLable;

public class Test2 {

	public void test() {
		int x = 5;
		int y = 0;
		int z = 0;
		x = 3;
		if (y != 0)
			x = 1;

		//x = 3;

		if(x != 3)
			ErrorLable.Error();
	}

}
