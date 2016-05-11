package playWithZ3;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;

public class TestTree2 {
	public static void main(String[] args) {
		InterpolationContext ictx= new InterpolationContext();
		Expr x=ictx.mkIntConst("x");
		Expr y=ictx.mkIntConst("y");
		Expr z=ictx.mkIntConst("z");
		Expr c=ictx.mkIntConst("c");
		Expr d=ictx.mkIntConst("d");
		IntExpr one=ictx.mkInt(1);
		BoolExpr f1=ictx.mkEq(x, one);
		BoolExpr f2=ictx.mkEq(y, x);
		BoolExpr f3=ictx.mkEq(c, d);
		BoolExpr[] result1=new BoolExpr[3];
		result1[0]=f1;
		result1[1]=f2;
		result1[2]=f3;
		Expr y_plus_one=ictx.mkAdd((ArithExpr)y,one);
		BoolExpr ff1=ictx.mkEq(y, y_plus_one);
		BoolExpr ff2=ictx.mkEq(d, z);
		BoolExpr[] result2=new BoolExpr[2];
		result2[0]=ff1;
		result2[1]=ff2;
		BoolExpr intF2=ictx.MkInterpolant(ff1);
		BoolExpr andfF2=ictx.mkAnd(ff2,intF2);
		intF2=ictx.MkInterpolant(andfF2);
		
	}
}
