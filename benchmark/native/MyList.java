import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.Iterator;

public class MyList<T> implements Iterable<T> {
  class MyIterator implements Iterator<T> {
    private int idx;

    MyIterator() {
      idx = 0;
    }

    public boolean hasNext() {
      return idx < size;
    }

    public T next() {
      return arr[idx];
    }
  }

  public Iterator<T> iterator() {
    return new MyIterator();
  }

  private int size;
  private int cap;
  private T[] arr;

  MyList(int cap) {
    this.size = 0;
    this.cap = cap;
    @SuppressWarnings("unchecked")
    T[] temp = (T[])(new Object[cap]);
    this.arr = temp;
  }

  MyList() {
    this(5);
  }

  MyList(MyList<T> other) {
    for (int i = 0; i < other.size(); ++i) {
      add(other.get(i));
    }
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
    for (int i = size-2; i > idx; --i) {
      arr[i] = arr[i-1];
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
