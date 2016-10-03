public class Test {
  public static void ensure(boolean b) { }
  public static void main(String[] args) {
    int x = 3;
    if (x == 3) {
      x = 4;
    }
    ensure(x == 4);


    x = 5;
    if (x == 6) {
      x = 6;
    }
    ensure(x == 6);
  }
}
