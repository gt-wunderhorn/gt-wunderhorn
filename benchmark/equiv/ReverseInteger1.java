public class ReverseInteger1 {
  static public int reverse1(int x) {
    int res = 0;
    while (x > 0) {
      int mod = x % 10;
      x = x / 10;
      res = res * 10 + mod;
    }
    return res;
  }

  static public int reverse2(int x) {
    int rev = 0;
    while(x != 0){
      rev = rev*10 + x%10;
      x = x/10;
    }

    return rev;
  }
}
