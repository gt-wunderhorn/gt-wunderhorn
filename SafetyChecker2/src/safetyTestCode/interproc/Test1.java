package safetyTestCode.interproc;

import safetyChecker.utilities.ErrorLable;

public class Test1 {

	public void test(int n) {
		int j = 2;
		int k = 3;
		int i = getNumber(j, k);
		if(i == 5) 
			ErrorLable.Error();

	}

	public int getNumber(int j, int k){
			
		int i = j + k;

		return i;
	}

}
