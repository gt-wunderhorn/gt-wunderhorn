public class Loop {
  public static int f(int x) {
    int sum = 0;
    while (x > 0) {
      sum++;
      x--;
    }
    return sum;
  }

  public static int g(int x) {
    return x;
  }
}
