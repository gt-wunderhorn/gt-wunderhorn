

public class LinkedList1 {
	static public void test(int length){
		LinkedList a=new LinkedList();
		for(int i=0;i<length;i++){
			a.add(i);
		}
		boolean checkPassword=true;
		for(int i=0;i<length;i++){
			if(a.get(i)<0){
				checkPassword=false;
			}
		}
		ErrorFunction.Error3(checkPassword);
	}
}
