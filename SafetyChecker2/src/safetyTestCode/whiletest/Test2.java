package safetyTestCode.whiletest;

import safetyChecker.ErrorLable;

public class Test2 {

	public void test() {
		int x = 5;
		int y = 1;


//		for(int i = 0; i < 3; i++)
//			for(int j = 0; j < 2; j++)
//				y++;
					    
//		int y = 0;
//		if(y == 0)
//			x = 0;
//		if(y == 5) 
//			x = 5;


		for(int i = 0; i < 100; i++)
			for(int j = 0; j < 100; j++) {}		

	//	if(y == x+2)
			ErrorLable.Error();
	}

}
