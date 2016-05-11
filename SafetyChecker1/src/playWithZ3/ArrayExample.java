package playWithZ3;

import z3_helper.NewSort;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Constructor;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Solver;
import com.microsoft.z3.Sort;
import com.microsoft.z3.Status;

public class ArrayExample {
	public static void main(String[] args) {
		InterpolationContext ictx = new InterpolationContext();
		ArrayExpr one=ictx.mkArrayConst("array1", ictx.getIntSort(),  ictx.getIntSort());
		ArrayExpr two=ictx.mkArrayConst("array2", ictx.getIntSort(), ictx.getIntSort());
		
	}
}
