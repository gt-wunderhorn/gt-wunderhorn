public class MyListTest {
  public static void printList(MyList<Integer> is) {
    for (int i = 0; i < is.size(); ++i) {
      System.out.println(is.get(i));
    }
    System.out.println();
  }

  public static void main(String[] args) {
    MyList<Integer> is = new MyList<Integer>();

    is.add(5);
    is.add(1);
    is.add(6);
    is.add(7);
    is.add(3);
    is.add(0);
    is.add(4);
    is.add(2);

    printList(is);
    MyArrays.sort(is);
    printList(is);
    MyArrays.reverse(is);
    printList(is);
  }
}
