package z3_helper;

import java.util.Map;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;

import soot.Local;
import soot.RefType;
import soot.Type;
import soot.Unit;
import soot.Value;
import soot.jimple.InstanceInvokeExpr;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;
import soot.jimple.NullConstant;
import soot.jimple.ReturnStmt;
import soot.jimple.StringConstant;
import soot.jimple.internal.JimpleLocal;

public class InvokeHelper {
	public static BoolExpr CallToZ3(InvokeExpr v, PathHelper thePathHelper,
			PathCoverter thePathCoverter, int Level, int nodeIndex) {
		int numberOfParameter = v.getArgCount();
		BoolExpr[] result = new BoolExpr[numberOfParameter];
		int index = numberOfParameter - 1;
		for (int i = index; i > -1; i--) {
			Value right = v.getArg(i);
			if (right instanceof NullConstant){
				result[i]=thePathCoverter.getIctx().mkTrue();
				continue;
			}
			Type rightType = right.getType();
			String name = "Parameter" + i + "Level" + Level;
			Local left = new JimpleLocal(name, right.getType());
			if (right instanceof StringConstant) {

			}
			Expr rightZ3 = thePathHelper.CovertValue(right, false, nodeIndex);
			Expr leftZ3 = thePathHelper.CovertValue(left, true, nodeIndex);
			Type leftType = left.getType();
			BoolExpr leftEqRight = thePathHelper.CovertAssignStmt(rightZ3,
					leftZ3, leftType, left, nodeIndex);
			if (right instanceof StringConstant) {
				StringConstant s = (StringConstant) right;
				BoolExpr half = StringHelper.covertString(s, thePathCoverter,
						rightZ3);
				result[i] = thePathCoverter.getIctx().mkAnd(leftEqRight, half);
			} else {
				result[i] = leftEqRight;
			}
			Expr paraZ3 = thePathHelper.CovertValue(left, false, nodeIndex);
			// it means parameter is not model in global variable (not
			// object
			// and field),so we need add this to next level local set
			thePathCoverter.pushPara(paraZ3);

		}
		BoolExpr passParameter =null;
		if(result==null){
			passParameter=thePathCoverter.getIctx().mkTrue();
		}
		else{
		 passParameter = thePathCoverter.getIctx().mkAnd(result);
		}
		if (v instanceof InstanceInvokeExpr) {
			Value right = ((InstanceInvokeExpr) v).getBase();
			Type rightType = right.getType();
			String name = "thisParameter";
			Local left = new JimpleLocal(name, right.getType());
			Expr rightZ3 = thePathHelper.CovertValue(right, false, nodeIndex);
			Expr leftZ3 = thePathHelper.CovertValue(left, true, nodeIndex);
			Type leftType = left.getType();
			BoolExpr leftEqRight = thePathHelper.CovertAssignStmt(rightZ3,
					leftZ3, leftType, left, nodeIndex);
			Expr paraZ3 = thePathHelper.CovertValue(left, false, nodeIndex);
			// it means parameter is not model in global variable (not
			// object
			// and field),so we need add this to next level local set
			thePathCoverter.pushPara(paraZ3);
			passParameter = thePathCoverter.getIctx().mkAnd(leftEqRight,
					passParameter);
		}
		return passParameter;
	}

	public static Expr returnToZ3(PathCoverter thePathCoverter) {
		return thePathCoverter.popReturn();
	}

	public static BoolExpr returnStmt(ReturnStmt v, PathHelper thePathHelper,
			PathCoverter thePathCoverter, int level, int nodeIndex) {
		Value right = v.getOp();
		String name = thePathCoverter.getRenameString(level) + "Return";
		Local left = new JimpleLocal(name, right.getType());
		Expr rightZ3 = thePathHelper.CovertValue(right, false, nodeIndex);
		Expr leftZ3 = thePathHelper.CovertValue(left, true, nodeIndex);
		Type leftType = left.getType();
		BoolExpr leftEqRight = thePathHelper.CovertAssignStmt(rightZ3, leftZ3,
				leftType, left, nodeIndex);
		Expr returnZ3 = thePathHelper.CovertValue(left, false, nodeIndex);
        if (right instanceof StringConstant){
			StringConstant sConstant=(StringConstant) right;
			BoolExpr constrains=StringHelper.covertString(sConstant, thePathCoverter, rightZ3);
			leftEqRight=thePathCoverter.getIctx().mkAnd(leftEqRight,constrains);
		}
		thePathCoverter.pushReturn(returnZ3);
		return leftEqRight;
	}

	public static void returnVoid(PathCoverter thePathCoverter) {
		thePathCoverter.pushReturn(thePathCoverter.getIctx().mkTrue());
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
