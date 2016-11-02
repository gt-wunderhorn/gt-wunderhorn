public class MyArrays {
  @SuppressWarnings("unchecked")
  static <T> MyList<T> asList(T... a) {
    MyList<T> list = new MyList<T>();
    for (int i = 0; i < a.length; ++i) {
      list.add(a[i]);
    }
    return list;
  }

  static void fill(boolean[] a, boolean val) {
    for (int i = 0; i < a.length; ++i) {
      a[i] = val;
    }
  }
  static void fill(int[] a, int val) {
    for (int i = 0; i < a.length; ++i) {
      a[i] = val;
    }
  }
  static void fill(long[] a, long val) {
    for (int i = 0; i < a.length; ++i) {
      a[i] = val;
    }
  }
  static void fill(short[] a, short val) {
    for (int i = 0; i < a.length; ++i) {
      a[i] = val;
    }
  }
  static void fill(float[] a, float val) {
    for (int i = 0; i < a.length; ++i) {
      a[i] = val;
    }
  }
  static void fill(double[] a, double val) {
    for (int i = 0; i < a.length; ++i) {
      a[i] = val;
    }
  }

  static <T> void reverse(MyList<T> list) {
    for (int i = 0, j = list.size() - 1; i < j; i++) {
      T temp = list.remove(j);
      list.add(i, temp);
    }
  }

  static void sort(MyList<Integer> list) {
    int size = list.size();
    MyList<Integer> storage = new MyList<Integer>();
    for (int i = 0; i < size; ++i) {
      storage.add(list.get(i));
    }
    list.clear();

    for (int i = 0; i < size; ++i) {
      Integer m = min(storage);
      int idx = storage.indexOf(m);
      storage.remove(idx);
      list.add(m);
    }
  }

  static Integer min(MyList<Integer> list) {
    if (list.isEmpty()) {
      return null;
    }

    Integer min = list.get(0);
    for (int i = 1; i < list.size(); ++i) {
      if (list.get(i) < min) {
        min = list.get(i);
      }
    }
    return min;
  }

  static Integer max(MyList<Integer> list) {
    if (list.isEmpty()) {
      return null;
    }

    Integer max = list.get(0);
    for (int i = 1; i < list.size(); ++i) {
      if (list.get(i) > max) {
        max = list.get(i);
      }
    }
    return max;
  }
}
