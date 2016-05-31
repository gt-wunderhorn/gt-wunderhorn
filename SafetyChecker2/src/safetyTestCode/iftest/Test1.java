package safetyTestCode.iftest;

import infoFlow.ErrorLable;

public class Test1 {

	public void test() {
		int x = 5;
		int y = 3;
		x = y+1;
		x = x-1;
		if(x != 3)
			ErrorLable.Error();
	}

}
