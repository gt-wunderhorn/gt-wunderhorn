import java.io.IOException;
import java.util.HashMap;
import java.util.Scanner;

class Test {
  static HashMap<Long,Long> map = new HashMap<Long,Long>();

  public static void main(String[] args) throws IOException {
    Scanner sc = new Scanner(System.in);

    for(int i = 1; i <= 10; i++)    {
      long exchg = sc.nextInt();
      long sum = 0;
      sum = calculaCoin(exchg);
      System.out.println(sum);
    }
  }

  public static long calculaCoin(long n) {
    long sum = 0;
    if(n > 11){
      if(map.containsKey(n)){
        sum =  map.get(n);
      } else {
        sum = calculaCoin(n/2) + calculaCoin(n/3) + calculaCoin(n/4);
      }
    } else {
      sum = n;
    }

    map.put(n, sum);

    return n>sum?n:sum;
  }
}
