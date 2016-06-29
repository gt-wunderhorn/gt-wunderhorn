package safetyTestCode.object;

import safetyChecker.ErrorLable;

public class Test1 {

	public void test() {
		Obj o = new Obj();
		byte b = 3;
		short s = 2;
		int i = b;
		long l1 = 23;
		long l = i;
//		o.access1 = a;
//		o.access2 = b;
//		int x = o.access1;

		if(l == l1-20)
			ErrorLable.Error();
	}
}
