package safetyTestCode.object;

import safetyChecker.ErrorLable;

public class Test1 {

	public void test() {
		Obj o = new Obj();
		o.access1 = 1;
		int x = o.access1;

		if(x == 3)
			ErrorLable.Error();
	}
}
