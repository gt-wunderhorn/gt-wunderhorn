import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

class MyList<T> {
  private int size;
  private int cap;
  private T[] arr;

  MyList() {
    size = 0;
    cap = 5;
    @SuppressWarnings("unchecked")
    T[] temp = (T[])(new Object[cap]);
    arr = temp;
  }

  int size() {
    return size;
  }

  boolean add(T t) {
    if (size == cap) {
      cap *= 2;
      @SuppressWarnings("unchecked")
      T[] temp = (T[])(new Object[cap]);
      for (int i = 0; i < size; ++i) {
        temp[i] = arr[i];
      }
      arr = temp;
    }
    arr[size] = t;
    size++;
    return true;
  }

  void add(int idx, T element) {
    add(get(size-1));
    for (int i = idx; i < size - 2; ++i) {
      arr[i+1] = arr[i];
    }
    arr[idx] = element;
  }

  boolean addAll(MyList<T> other) {
    for (int i = 0; i < other.size; ++i) {
      add(other.get(i));
    }
    return true;
  }

  boolean addAll(int idx, MyList<T> other) {
    for (int i = 0; i < other.size; ++i) {
      add(idx + i, other.get(i));
    }
    return true;
  }

  int indexOf(T t) {
    for (int i = 0; i < size; ++i) {
      if (arr[i] == t) {
        return i;
      }
    }
    return -1;
  }

  T get(int idx) {
    return arr[idx];
  }

  boolean contains(T t) {
    return indexOf(t) >= 0;
  }

  boolean containsAll(MyList<T> other) {
    for (int i = 0; i < other.size; ++i) {
      if (!contains(other.get(i))) {
        return false;
      }
    }
    return true;
  }

  boolean isEmpty() {
    return size == 0;
  }

  T set(int idx, T t) {
    T temp = arr[idx];
    arr[idx] = t;
    return temp;
  }

  boolean remove(T t) {
    int idx = indexOf(t);
    if (idx < 0) {
      return false;
    }
    remove(idx);
    return true;
  }

  T remove(int idx) {
    T temp = arr[idx];
    size--;
    for (int i = idx; i < size; ++i) {
      arr[i] = arr[i+1];
    }
    return temp;
  }

  boolean removeAll(MyList<T> other) {
    for (int i = 0; i < other.size; ++i) {
      if (!remove(other.get(i))) {
        return false;
      }
    }
    return true;
  }

  void clear() {
    size = 0;
  }
}
