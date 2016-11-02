import java.io.BufferedInputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.lang.Math;

public class Test {
  public static int ipSize;
  public static int matrixSize;
  public static int[] myRowarr;
  public static int[] myColarr;

  public static void calcPath(int N,int[] row, int[] col ){
    int pathLength = 0;
    for (int i =1; i< N*N; i++){
      pathLength +=  Math.abs(myRowarr[i] - myRowarr[i+1]);
      pathLength +=  Math.abs(myColarr[i] - myColarr[i+1]);
    }
    System.out.println(pathLength);
  }

  public static void main(String[] args) {
    Scanner stdin = new Scanner(new BufferedInputStream(System.in));
    ArrayList<Integer> ipArr = new ArrayList<Integer>();

    ipArr.add(stdin.nextInt());

    // while (stdin.hasNextInt()){
    //   ipArr.add(stdin.nextInt());
    // }
    // ipSize = Integer.valueOf(ipArr.get(0));
    // int k = 1;

    // for(int ip = 0; ip< ipSize; ip++){
    //   matrixSize = Integer.valueOf(ipArr.get(k));
    //   myRowarr = new int[matrixSize*matrixSize +1];
    //   myColarr = new int[matrixSize*matrixSize +1];
    //   int i = 1;
    //   int j = 1;

    //   for(i = 1; i <= matrixSize; i++) {

    //     for(j = 1; j <= matrixSize; j++){
    //       k ++;
    //       myRowarr[Integer.valueOf(ipArr.get(k))] = i;
    //       myColarr[Integer.valueOf(ipArr.get(k))] = j;

    //     }

    //   }
    //   calcPath(matrixSize, myRowarr,  myColarr);
    //   k++;
    //   myRowarr = null;
    //   myColarr = null;
    // }
  }
}
