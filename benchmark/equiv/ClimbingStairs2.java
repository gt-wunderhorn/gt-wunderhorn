public class ClimbingStairs2 {
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

  static public int climbStairs3(int n) {
    int p1 = 1, p2=1, curr=1;

    for(int i=2;i<=n;i++){
      curr = p1 + p2;
      p2 = p1;
      p1 = curr;
    }
    return curr;
  }
}
