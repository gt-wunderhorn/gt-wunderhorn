public class AddDigits1 {
  static public int addDigits1(int num) {
    int result = num - 9 * ((num - 1) / 9);
    return result;
  }

  static public int addDigits2(int num) {
    int result = num % 9 == 0 ? 9 : (num % 9);
    return result;
  }
}
