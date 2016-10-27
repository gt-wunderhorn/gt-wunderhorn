import java.util.Scanner;

class Test {
  public static void ensure(boolean b) { }
  public static void main(String args[])
  {
    Scanner sc = new Scanner(System.in);
    int t = sc.nextInt();
    while(t-->0)
    {
      int[] arr = new int[10];
      for(int i=0;i<10;i++)
      {
        ensure(i < arr.length);
        arr[i] = sc.nextInt();
      }

      int min = 10;
      int index = 0;
      for(int i=0;i<10;i++)
      {
        if(arr[i]<min)
        {
          min = arr[i];
          index = i;
        }
        else if(arr[i]==min && index==0)
          index = i;
      }
      if(index==0)
        System.out.print(1);
      for(int i=0;i<=min;i++)
      {
        System.out.print(index);
      }
      System.out.println();
    }
  }
}

