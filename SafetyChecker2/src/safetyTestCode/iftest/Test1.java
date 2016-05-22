package safetyTestCode.iftest;

import infoFlow.ErrorLable;

public class Test1 {

	public void test() {
		int x = 5;

		x = 4;
		if(x == 5)
			ErrorLable.Error();
	}

}
