public class TrailingZeroes4 {
  static public int trailingZeroes2(int n) {
    int sum = 0;
    while (n >= 5) {
      sum += n / 5;
      n = n / 5;
    }
    return sum;
  }

  static public int trailingZeroes4(int n) {
    int x = 0;
    int y = n / 5;
    while (y!=0) {
      x = x + y;
      y = y / 5;
    }
    return x;
  }
}
