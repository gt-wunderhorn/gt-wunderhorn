public class Test {
  public static void ensure(boolean b) { }
  static class Element {
    Element next;
    Element() { next = null; }
  }

  public static void main(String[] args) {
    Element head = new Element();
    Element tail = head;
    Element e = null;

    for (int i = 0; i < 10; ++i) {
      e = new Element();
      tail.next = e;
      tail = e;
    }

    e = head;
    while (e.next != null) {
      e = e.next;
    }

    ensure(e == tail);
  }
}
