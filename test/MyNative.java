class MyNative {
  public static void entry() {
    arraycopy(null, 0, null, 0, 0);
  }

  public static void arraycopy(
      Object src,
      int srcPos,
      Object dest,
      int destPos,
      int length) {

    Object[] src_ = (Object[])src;
    Object[] dest_ = (Object[])dest;

    for (int i = 0; i < length; ++i) {
      dest_[destPos+i] = src_[srcPos +i];
    }
  }
}
