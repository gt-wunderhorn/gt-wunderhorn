package safetyTestCode.interproc;

public class Rob {

	public int rob(int[] num) {

		if(num == null || num.length == 0)
			return 0;
		int len = num.length;
		if(len == 1)
			return num[0];
	        return Math.max(robInt(num, 0, len - 2), robInt(num, 1, len - 1));
	
	}

	private int robInt(int[] num, int low, int high){
		    int pre = 0, cur = 0;
		    for(int i = low; i <= high; i++){
			    int next = Math.max(pre + num[i], cur);
			    pre = cur;
			    cur = next;			 
		    }
		    return cur;
	}


}
