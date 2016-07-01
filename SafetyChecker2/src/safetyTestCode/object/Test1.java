package safetyTestCode.object;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.Scanner;

import safetyChecker.ErrorLable;

public class Test1 {

	public static void main(String[] args) {
//		int[] intArray = new int[2];
//		intArray[0] = 3;

		int n=1;
		int[][] intArray3 = new int[3][2];
		intArray3[n][2] = 3;
		if(intArray3[0][1] != 3)
			ErrorLable.Error();


	}

	public static void main4(String[] args) {
		Scanner sc = new Scanner(System.in);
		String A = sc.next();
		boolean flag = true;
		for (int i = 0; i < A.length() / 2; ++i) {
			if (A.charAt(i) == A.charAt(A.length() - i - 1))
				continue;
			else {
				System.out.println("No");
				flag = false;
				break;
			}
		}
		if (!flag)
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
		// o.access1 = a;
		// o.access2 = b;
		// int x = o.access1;

		if (l == l1 - 20)
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
			// for (int i = 1; i < n; i++)
			// if (speed >= aa[i]) {
			// speed = aa[i];
			// cnt++;
			// }
			pw.println(cnt);
		}
		pw.close();

		if (a == 11)
			ErrorLable.Error();
	}

	public static void main3(String[] args) throws IOException {

		int a = 3;
		int b = 5;
		int i = Math.max(29, b);
		int j = Math.max(b, a);
		int c = i % j;
		if (c != 4)
			ErrorLable.Error();
	}
}
