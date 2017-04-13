public class TrailingZeroes3 {
  static public int trailingZeroes1(int n) {
    int num;
    for (num = 0; n != 0; n /= 5, num += n)
      ;
    return num;
  }

  static public int trailingZeroes3(int n) {
    int s = 0;
    while (n > 4)
      s += (n /= 5);
    return s;
  }
}
