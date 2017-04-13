public class TrailingZeroes2 {
  static public int trailingZeroes1(int n) {
    int num;
    for (num = 0; n != 0; n /= 5, num += n)
      ;
    return num;
  }

  static public int trailingZeroes2(int n) {
    int sum = 0;
    while (n >= 5) {
      sum += n / 5;
      n = n / 5;
    }
    return sum;
  }
}
