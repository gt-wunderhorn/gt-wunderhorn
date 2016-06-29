package safetyTestCode.object;

import safetyChecker.ErrorLable;

public class Test1 {

	public void test() {
		Obj o = new Obj();
		int a = 3;
		int b = 4;
		o.access1 = a;
		o.access2 = b;
		int x = o.access1;

		if(o.access1 == o.access2-1)
			ErrorLable.Error();
	}
}
