
public class ClimbingStairs1 {
  static public int climbStairs1(int n) {
    int count1 = 1;
    int count2 = 1;

    for (int i = 2; i <= n; i++) {
      int temp = count2;
      count2 = temp + count1;
      count1 = temp;
    }
    return count2;
  }

  static public int climbStairs2(int n) {
    int sum = 2, prev = 1, curr = 0;
    for (int i = 2; i < n; i++) {
      curr = sum;
      sum += prev;
      prev = curr;
    }
    return sum;
  }
}
