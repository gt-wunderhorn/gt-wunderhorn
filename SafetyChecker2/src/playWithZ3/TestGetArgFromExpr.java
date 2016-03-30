package playWithZ3;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Params;
import com.microsoft.z3.Solver;
import com.microsoft.z3.InterpolationContext.ComputeInterpolantResult;
import com.microsoft.z3.Status;
import com.microsoft.z3.enumerations.Z3_lbool;

public class TestGetArgFromExpr {
	public static void main(String[] args) {
        System.out.println("SimpleExample");

        {   
        	InterpolationContext ictx= new InterpolationContext();
        	IntExpr a = ictx.mkIntConst("a");
    		IntExpr one=ictx.mkInt(1);
    		IntExpr b = ictx.mkIntConst("b");
    		IntExpr two=ictx.mkInt(2);
    		BoolExpr a_eq_one=ictx.mkEq(a, one);
    		BoolExpr b_eq_two=ictx.mkEq(b, two);
    		BoolExpr f1=ictx.mkAnd(a_eq_one,b_eq_two);
    		IntExpr c=ictx.mkIntConst("c");
    		IntExpr d=ictx.mkIntConst("d");
    		Expr[] original=new Expr[2];
    		original[0]=a;
    		original[1]=b;
    		Expr[] to=new Expr[2];
    		to[0]=c;
    		to[1]=d;
    		IntExpr a1 = ictx.mkIntConst("a");
    		IntExpr b1= ictx.mkIntConst("b");
    		original[0]=a1;
    		original[1]=a1;
    		BoolExpr f2=(BoolExpr) f1.substitute(original, to);
    		System.out.println(f1.toString());
    		System.out.println(f2.toString());
    		BoolExpr a1_eq_a=ictx.mkEq(a, a1);
    		BoolExpr notf1=ictx.mkNot(a1_eq_a);
    		Solver s=ictx.mkSolver();
    		s.add(notf1);
    		Status s1=s.check();
    		System.out.println(s1);
    		
    		//s.add(and_1);
    		//s.add(and_2);
    		//Status status = s.check();
    		//System.out.println(status);
    		ictx.dispose();
            
        }
}
}
