package safetyTestCode.interproc;

import safetyChecker.utilities.ErrorLable;

public class Test1 {

	public void test(int n) {
		int j = 1;
		int k = 5;
		int i = getNumber(j, k);
		if(i == 11) 
			ErrorLable.Error();

	}

	public int getNumber(int j, int k){
			
		int i = j + getDouble(k);

		return i;
	}


	int getDouble(int k) { return k*2; }

}
