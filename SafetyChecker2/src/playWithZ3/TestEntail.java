package playWithZ3;

import z3_helper.Z3Utility;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Params;
import com.microsoft.z3.Solver;
import com.microsoft.z3.Status;
import com.microsoft.z3.InterpolationContext.ComputeInterpolantResult;

public class TestEntail {
	public static void main(String[] args) {
		InterpolationContext ictx= new InterpolationContext();
		boolean result=Z3Utility.checkEntail(ictx.mkTrue(), ictx.mkFalse(), ictx);
		System.out.println(result);
		IntExpr a = ictx.mkIntConst("a");
		IntExpr b = ictx.mkIntConst("b");
		IntExpr one=ictx.mkInt(1);
		IntExpr zero=ictx.mkInt(0);
		BoolExpr a_gt_zero=ictx.mkGt(a, one);
		BoolExpr a_gt_one=ictx.mkGt(a, zero);
		BoolExpr b_gt_zero=ictx.mkGt(b, one);
		BoolExpr b_gt_one=ictx.mkGt(b, zero);
		BoolExpr f1=ictx.mkAnd(a_gt_zero,b_gt_one);
		BoolExpr f2=ictx.mkAnd(a_gt_one,b_gt_zero);
		BoolExpr notF1=ictx.mkNot(f1);
		BoolExpr notF2=ictx.mkNot(f2);
		BoolExpr f1EntailF2=ictx.mkAnd(f1,notF2);
		BoolExpr f2EntailF1=ictx.mkAnd(notF1,f2);
		Solver s=ictx.mkSolver();
		s.reset();
		
		
		
	}
}
