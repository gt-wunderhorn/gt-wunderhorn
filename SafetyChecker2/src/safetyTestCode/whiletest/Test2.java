package safetyTestCode.whiletest;

import safetyChecker.ErrorLable;

public class Test2 {

	public void test2() {
		int x = 5;
		int y = 1;

		boolean a = true;
		while(a)
			x++;

//		for(int i = 0; i < 2; i++)
//			for(int j = 0; j < 1; j++)
//				y++;
					    
//		int y = 0;
//		if(y == 0)
//			x = 0;
//		if(y == 5) 
//			x = 5;


//		for(int i = 0; i < 2; i++)
//			for(int j = 0; j < 5; j++) {}		

	//	if(y == x+2)
			ErrorLable.Error();
	}

	public void test(int n) {
		for(int i = 0; i < 2; i++) {
			for(int j = 0; j < 2; j++) {}
		}
		ErrorLable.Error();
	}


}
