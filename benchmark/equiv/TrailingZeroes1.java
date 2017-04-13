public class TrailingZeroes1 {
  static public int trailingZeroes2(int n) {
    int sum = 0;
    while (n >= 5) {
      sum += n / 5;
      n = n / 5;
    }
    return sum;
  }

  static public int trailingZeroes3(int n) {
    int s = 0;
    while (n > 4)
      s += (n /= 5);
    return s;
  }
}
