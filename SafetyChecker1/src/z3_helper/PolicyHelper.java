package z3_helper;

import infoFlow.HelpTree;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

import soot.Unit;
import soot.Value;
import soot.jimple.InvokeStmt;
import soot.jimple.StringConstant;

public class PolicyHelper {
	static public void storeResult(InvokeStmt u, PathHelper theHelper,
			PathCoverter theCoverter) {
		int count = u.getInvokeExpr().getArgCount();
		for (int i = 0; i < count; i++) {
			Value right = u.getInvokeExpr().getArg(i);
			Expr rightZ3 = theHelper.CovertValue(right, false, 0);
			theCoverter.updateErrorCheck(i, rightZ3);
		}
	}

	static public BoolExpr storeSink(InvokeStmt u, PathHelper theHelper,
			PathCoverter theCoverter) {
		String signature = HelpTree.getMethodSignature(u);
		if (signature.contains("sendTextMessage")) {
			Value right = u.getInvokeExpr().getArg(2);
			Expr rightZ3 = theHelper.CovertValue(right, false, 0);
			String name = "realString";
			ArrayExpr realString = (ArrayExpr) theCoverter.getGlobal(name);
			/**System.out.println("which one is null");
			System.out.println(rightZ3);
			System.out.println(realString);**/
			Expr f1 = theCoverter.getIctx().mkSelect(realString, rightZ3);
			int size = theCoverter.getErrorCheck().size();
			theCoverter.updateErrorCheck(size, f1);
			return theCoverter.getIctx().mkTrue();
		}
		if (signature.contains("android.util.Log")) {
			Value right = u.getInvokeExpr().getArg(1);
			if (right instanceof StringConstant) {
				StringConstant sConstant = (StringConstant) right;
				String oldname = "realString";
				InterpolationContext ictx = theCoverter.getIctx();
				Sort arraySort = ictx.mkArraySort(ictx.getIntSort(),
						ictx.getIntSort());
				int StringSize = theCoverter.getRealArraySize(oldname);
				ArrayExpr theString = ictx.mkArrayConst("string" + StringSize+theCoverter.getPathNumber(),
						ictx.getIntSort(), ictx.getIntSort());
				int size = theCoverter.getErrorCheck().size();
				theCoverter.updateErrorCheck(size, theString);
				return covertString(sConstant, theCoverter, theString);
			}
			else{
				right = u.getInvokeExpr().getArg(1);
				Expr rightZ3 = theHelper.CovertValue(right, false, 0);
				String name = "realString";
				ArrayExpr realString = (ArrayExpr) theCoverter.getGlobal(name);
				/**System.out.println("which one is null");
				System.out.println(rightZ3);
				System.out.println(realString);**/
				Expr f1 = theCoverter.getIctx().mkSelect(realString, rightZ3);
				int size = theCoverter.getErrorCheck().size();
				theCoverter.updateErrorCheck(size, f1);
				return theCoverter.getIctx().mkTrue();
			}
		}
		return theCoverter.getIctx().mkTrue();
	}

	static BoolExpr covertString(StringConstant s, PathCoverter theCoverter,
			ArrayExpr theString) {
		String a = s.value;
		InterpolationContext ictx = theCoverter.getIctx();
		BoolExpr[] result = new BoolExpr[a.length()];
		for (int i = 0; i < a.length(); i++) {
			int theOne = a.charAt(i);
			IntExpr theOneZ3 = ictx.mkInt(theOne);
			IntExpr indexZ3 = ictx.mkInt(i);
			Expr select = ictx.mkSelect(theString, indexZ3);
			result[i] = ictx.mkEq(select, theOneZ3);
		}
		BoolExpr constrainsAboutArray = ictx.mkAnd(result);
		return constrainsAboutArray;
	}
}
