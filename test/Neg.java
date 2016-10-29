import java.util.Scanner;
class Test
{
  public static void main(String args[])
  {
    int i,n;
    Scanner sc= new Scanner(System.in);
    n=sc.nextInt();
    while (n < 0) {
      n=sc.nextInt();
    }
    int a[]=new int[n];
    for(i=0;i<n;i++) {
      a[i] = 5;
    }
    for(i=0;i<n;i++) {
      System.out.println(a[i]);
    }
  }
}
