package playWithSoot;

import java.util.ArrayList;

public class TestArrayList {
	public static void main(String[] args) {
		ArrayList<ArrayList<Integer>> result = new ArrayList<ArrayList<Integer>>();
		ArrayList<Integer> a = new ArrayList<Integer>();
		result.add(a);
		a.add(1);
		a.add(2);
		a.add(3);
		ArrayList b = result.get(0);
		for (int i = 0; i < 3; i++) {
			System.out.println(b.get(i));
		}
	}
}
