public class AddDigits2 {
  static public int addDigits1(int num) {
    int result = num - 9 * ((num - 1) / 9);
    return result;
  }

  static public int addDigits3(int num) {

    while (num > 9)
      num = num / 10 + num % 10;

    return num;
  }
}
