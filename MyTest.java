public class MyTest {
  public static void main(String[] args) {
    lcm(0, 0);
  }

  static int gcd(int a, int b) {
    int t;
    while (b != 0) {
      t = a % b;
      a = b;
      b = t;
    }
    return a;
  }

  static int lcm(int a, int b) {
    return a / gcd(a, b) * b;
  }
}
