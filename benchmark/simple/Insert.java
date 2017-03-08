import java.util.Scanner;

public class Test {
  static void ensure(boolean b) { }

  static class Element {
    public Element next;
  }

  static int length(Element e) {
    int l = 0;
    while (e != null) {
      l = l + 1;
      e = e.next;
    }
    return l;
  } 

  static void insert(Element e, Element h, Element x) {
    int before = length(h);
    Element p = h;
    Element q = null;

    while (p != x && p != null) {
      q = p;
      p = p.next;
    }
    q.next = e;
    e.next = p;

    ensure(before + 1 == length(h));

    System.out.println(before);
    System.out.println(length(h));
  }

  public static void main(String[] args) {
    Element h = null;
    Element x = null;

    for (int i = 0; i < 15; ++i) {
      Element t = new Element();
      t.next = h;
      h = t;

      if (i == 7) {
        x = h;
      }
    }

    insert(new Element(), h, x);
  }
} 
