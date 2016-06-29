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
		for(int i = 0; i < 5; i++) {
			for(int j = 0; j < 5; j++) {}
		}
		// 2-2 error root #   28, L747-D37 -- benefit 1 root 20 vertexes 
		// 3-3 error root #  564, L15790-D60 -- 15 roots
		// 3-4 error root # 1895, L53414-D69 
		// 4-3 error root # 4690, L160416-D77
		// 4-4 error root # 18636 L527160-D86 -- 25 
		// 5-5 error root # 20856 L593214-D87 -- 36
		// 10-10
		
		ErrorLable.Error(); 
	}


}
