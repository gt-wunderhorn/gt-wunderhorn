package safetyTestCode.object;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;

import safetyChecker.ErrorLable;

public class Test1 {

	public static void main(String[] args) throws IOException {
		
		int a = 3;
		int b = 5;
		int i = Math.max(a,b);
		if(i == b-a)
			ErrorLable.Error();
	}	

	public void test() {
		Obj o = new Obj();
		byte b = 3;
		short s = 2;
		int i = b;
		long l1 = 23;
		long l = i;
		b = (byte) l1;
//		o.access1 = a;
//		o.access2 = b;
//		int x = o.access1;

		if(l == l1-20)
			ErrorLable.Error();
	}

        public static void main2(String[] args) throws IOException {
                BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
                PrintWriter pw = new PrintWriter(System.out);
                int t = Integer.parseInt(br.readLine().trim());
		int a = 2;

                while (t-- > 0) {
			a += t;
                        int n = Integer.parseInt(br.readLine().trim());
                        String[] ss = br.readLine().trim().split("\\s+");
                        int[] aa = new int[n];
                        for (int i = 0; i < n; i++)
                                aa[i] = Integer.parseInt(ss[i]);
                       int speed = aa[0];
                        int cnt = 1;
//                        for (int i = 1; i < n; i++)
//                                if (speed >= aa[i]) {
//                                        speed = aa[i];
//                                        cnt++;
//                                }
                       pw.println(cnt);
                }
                pw.close();
 
		if(a == 11)
		ErrorLable.Error();
        }
}
