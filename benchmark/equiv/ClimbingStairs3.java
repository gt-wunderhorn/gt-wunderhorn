public class ClimbingStairs3 {
  static public int climbStairs2(int n) {
    int sum = 2, prev = 1, curr = 0;
    for (int i = 2; i < n; i++) {
      curr = sum;
      sum += prev;
      prev = curr;
    }
    return sum;
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
