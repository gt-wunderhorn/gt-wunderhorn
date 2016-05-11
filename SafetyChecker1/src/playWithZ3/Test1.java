package playWithZ3;


import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.InterpolationContext.ComputeInterpolantResult;
import com.microsoft.z3.Log;
import com.microsoft.z3.Params;
import com.microsoft.z3.Solver;
import com.microsoft.z3.Sort;
import com.microsoft.z3.Status;
import com.microsoft.z3.enumerations.Z3_lbool;

public class Test1 {
	public static void main(String[] args) {
		        System.out.println("SimpleExample");

		        {   
		        	InterpolationContext ictx= new InterpolationContext();
		        	IntExpr a = ictx.mkIntConst("a");
		    		IntExpr b = ictx.mkIntConst("b");
		    		IntExpr c = ictx.mkIntConst("c");
		    		IntExpr d = ictx.mkIntConst("d");
		    		IntExpr e = ictx.mkIntConst("e");
		    		IntExpr fg= ictx.mkIntConst("fg");
		    		BoolExpr a_eq_b = ictx.mkEq(a, b);
		    		BoolExpr a_eq_c = ictx.mkEq(a, c);
		    		BoolExpr c_eq_d = ictx.mkEq(c, d);
		    		BoolExpr b_eq_e = ictx.mkEq(b, e);
		    		BoolExpr d_eq_e = ictx.mkEq(d, e);
		    		BoolExpr not_d_eq_e=ictx.mkNot(d_eq_e);
		    		BoolExpr f1 = ictx.mkAnd(a_eq_b, a_eq_c);
		    		BoolExpr f2 = c_eq_d;
		    		BoolExpr f3 = ictx.mkAnd(b_eq_e,not_d_eq_e);
		    		
		    		//s.add(and_1);
		    		//s.add(and_2);
		    		//Status status = s.check();
		    		//System.out.println(status);
		    		
		    		BoolExpr i1 = ictx.MkInterpolant(f1);
		    		System.out.println("i1: " + i1);
		    		BoolExpr pat1 = ictx.mkAnd(i1,f2);
		    		System.out.println("pat1: " + pat1);
		    		BoolExpr i2 = ictx.MkInterpolant(pat1);
		    		Expr pat2=ictx.mkAnd(i2,f3);
		    		System.out.println("pat2: " + pat2);
		    		Params params = ictx.mkParams();
		    		ComputeInterpolantResult result = ictx.ComputeInterpolant(pat2, params);
		    		Z3_lbool status = result.status;
		    		System.out.println(status);
		    		Expr[] args1=null;
		    		if (status == Z3_lbool.Z3_L_FALSE) {
		    			BoolExpr[] interps = result.interp;
		    			for(int i = 0; i < interps.length; i++) {
		    				System.out.println(interps[i]);
		    				args1=interps[i].getArgs();
		    				for(int j = 0; j < interps.length; j++){
		    					System.out.println(args1[j]);
		    				}
		    			}
		    		}
		    		ictx.dispose();
		            
		        }
	}
}
