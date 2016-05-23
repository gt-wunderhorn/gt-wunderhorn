package safetyTestCode.iftest;

import infoFlow.ErrorLable;

public class Test1 {

	public void test() {
		int x = 5;

		x = 5;
		if(x == 5)
			ErrorLable.Error();
	}

}
