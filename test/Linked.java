public class Test {
  public static void ensure(boolean b) { }
  static class Element {
    Element next;
    int val;
  }

  public static void main(String[] args) {
    Element head = null;
    Element tail = null;
    Element e;

    for (int i = 0; i < 10; ++i) {
      e = new Element();
      e.next = null;
      e.val = 1;
      if (tail != null) {
        tail.next = e;
      }
      tail = e;
      if (head == null) {
        head = tail;
      }
    }

    boolean a = true;
    while (head != null) {
      a = a && head.val == 1;
      head = head.next;
    }
    ensure(a);
  }
}
