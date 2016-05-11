package playWithZ3;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Params;
import com.microsoft.z3.InterpolationContext.ComputeInterpolantResult;
import com.microsoft.z3.enumerations.Z3_lbool;

public class TestTreeInterpolant {
	/**(declare-const x11 Int)
		(declare-const x21 Int)
		(declare-const x12 Int)
		(declare-const x22 Int)
		(declare-const x1 Int)
		(declare-const x2 Int)
		(compute-interpolant
    	(and
       (and (not (= x1 x2)) (or (and (= x1 x21) (= x2 x21)) (and (= x1 x22) (= x2 x22))))
       (interp(and (interp (= x11 0)) (= x21 (+ x11 1))))
       (interp(and (interp (= x12 0)) (= x22 (+ x12 1))))
       )
		)
	**/
	public static void main(String[] args) {
		InterpolationContext ictx= new InterpolationContext();
		Expr x11=ictx.mkIntConst("x11");
		Expr x21=ictx.mkIntConst("x21");
		Expr x12=ictx.mkIntConst("x12");
		Expr x22=ictx.mkIntConst("x22");
		Expr x1=ictx.mkIntConst("x1");
		Expr x2=ictx.mkIntConst("x2");
		IntExpr zero=ictx.mkInt(0);
		IntExpr one=ictx.mkInt(1);
		BoolExpr x11_eq_0 =ictx.mkEq(x11, zero);
		BoolExpr interp_x11_eq_0=ictx.MkInterpolant(x11_eq_0);
		Expr x11_add_1=ictx.mkAdd((ArithExpr)x11,(ArithExpr)one);
		BoolExpr x21_eq=ictx.mkEq(x21, x11_add_1);
		BoolExpr andF1=ictx.mkAnd(x21_eq,interp_x11_eq_0);
		BoolExpr interpF1=ictx.MkInterpolant(andF1);
		BoolExpr x12_eq_0 =ictx.mkEq(x12, zero);
		BoolExpr interp_x12_eq_0=ictx.MkInterpolant(x12_eq_0);
		Expr x12_add_1=ictx.mkAdd((ArithExpr)x12,(ArithExpr)one);
		BoolExpr x22_eq=ictx.mkEq(x22, x12_add_1);
		BoolExpr andF2=ictx.mkAnd(x22_eq,interp_x12_eq_0);
		BoolExpr interpF2=ictx.MkInterpolant(andF2);
		BoolExpr x1_eq_x21=ictx.mkEq(x1, x21);
		BoolExpr x2_eq_x21=ictx.mkEq(x2, x21);
		BoolExpr assign1=ictx.mkAnd(x1_eq_x21,x2_eq_x21);
		BoolExpr x1_eq_x22=ictx.mkEq(x1, x22);
		BoolExpr x2_eq_x22=ictx.mkEq(x2, x22);
		BoolExpr assign2=ictx.mkAnd(x1_eq_x22,x2_eq_x22);
		BoolExpr allAssign=ictx.mkOr(assign1,assign2);
		BoolExpr x1_eq_x2=ictx.mkEq(x1, x2);
		BoolExpr policy=ictx.mkNot(x1_eq_x2);
		BoolExpr andPolicy=ictx.mkAnd(x1_eq_x2,allAssign);
		BoolExpr[] path=new BoolExpr[2];
		path[0]=interpF1;
		path[1]=interpF2;
		BoolExpr pm1=ictx.mkAnd(path);
		BoolExpr pm=ictx.mkAnd(pm1,andPolicy);
		Params params = ictx.mkParams();
		ComputeInterpolantResult result = ictx.ComputeInterpolant(pm, params);
		Z3_lbool status = result.status;
		System.out.println(status);
		if (status == Z3_lbool.Z3_L_FALSE) {
			BoolExpr[] interps = result.interp;
			for(int i = 0; i < interps.length; i++) {
				System.out.println(interps[i]);
			}
		}
		ictx.dispose();
		
	}
	
}
