package safetyTestCode.object;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.Scanner;

import safetyChecker.utilities.ErrorLable;

public class Test1 {

	public static void main(String args[]) throws java.io.IOException {
		char a = 'b';
		String s1 = "sta";
		String s2 = "sym";

		if(a != s1.charAt(2))
			ErrorLable.Error();
	}

	public static void main7(String args[]) throws java.io.IOException {
		InputStreamReader isr = new InputStreamReader(System.in);
		BufferedReader br = new BufferedReader(isr);
		int j = 0, k = 0, i = 0, ch = Integer.parseInt(br.readLine());
		String s = "";
		for (i = 0; i < ch; i++) {
			int r = Integer.parseInt(br.readLine());
			int ar[][] = new int[r][r];
			for (j = 0; j < r; j++) {
				String str = br.readLine();
				str = str + " ";
				int n = 0, len = str.length(), l = 0;
				k = 0;
				while (l < len) {
					String str1 = "";
					while (str.charAt(l) != ' ')
						str1 = str1 + str.charAt(l++);
					if (str1 != "") {
						n = Integer.parseInt(str1);

						ar[j][k++] = n;
					}
					l++;
				}

				for (k = 0; k <= j; k++) {
					if (j == 0) {
						ar[j][k] = ar[j][k];
					} else if (k == 0) {
						ar[j][k] = ar[j][k] + ar[j - 1][k];
					} else if (j == k) {
						ar[j][k] = ar[j][k] + ar[j - 1][k - 1];
					} else {
						if ((ar[j][k] + ar[j - 1][k - 1]) > (ar[j][k] + ar[j - 1][k])) {
							ar[j][k] = ar[j][k] + ar[j - 1][k - 1];
						} else {
							ar[j][k] = ar[j][k] + ar[j - 1][k];
						}
					}

				}

			}

			int max = 0;
			for (j = 0; j < r; j++) {
				if (max < ar[r - 1][j]) {
					max = ar[r - 1][j];
				}
			}
			s = s + max + " ";

		}
		s = s.trim();
		for (i = 0; i < s.length(); i++) {
			if (s.charAt(i) != ' ')
				System.out.print(s.charAt(i));
			else
				System.out.println();
		}
		ErrorLable.Error();

	}

	public static void main6(String[] args) throws Exception {
		// int[] intArray = new int[2];
		// intArray[0] = 3;

//		InputStreamReader isr = new InputStreamReader(System.in);
//		BufferedReader br = new BufferedReader(isr);
//		int i = Integer.parseInt(br.readLine());
//		int j = Integer.parseInt(br.readLine());
		int[][] intArray3 = new int[1][2];

//		for(int i2 = 0; i2 < j; i2++)
//			for(int j2 = 0; j2 < j; j2++)
//				intArray3[i2][j2] = i2+j2;

		intArray3[0][1] = 3;
//		intArray3[1][2] = 7;
		if (intArray3[2][1] == 3)
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
