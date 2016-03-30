package z3_helper;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

import soot.Local;
import soot.Type;
import soot.Unit;
import soot.Value;
import soot.jimple.AssignStmt;
import soot.jimple.IntConstant;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;
import soot.jimple.NewExpr;
import infoFlow.Forest;
import infoFlow.HelpTree;

public class SpecialInvoke {
	public static boolean ifNosenseInvoke(Forest f, Unit u) {
		if (HelpTree.isInvoke(u)) {
			String signature = HelpTree.getMethodSignature(u);
			if (!f.getSubFunctionTree().containsKey(signature)) {
				return true;
			}
		}
		return false;
	}

	public static boolean ifSinkInvoke(Unit u) {
		if (HelpTree.isInvoke(u)) {
			String signature = HelpTree.getMethodSignature(u);
			if (signature.contains("sendTextMessage")) {
				return true;
			}
			if (signature.contains("android.util.Log")) {
				return true;
			}
		}
		return false;
	}

	public static boolean ifInput(Unit u) {
		if (HelpTree.isInvoke(u)) {
			String signature = HelpTree.getMethodSignature(u);
			if (signature.contains("getSimSerialNumber")) {
				return true;
			}
			if (signature.contains("getDeviceId()")) {
				return true;
			}
		}
		return false;
	}

	public static boolean ifNonSenseCompare(Unit u) {
		if (HelpTree.isInvoke(u)) {
			String signature = HelpTree.getMethodSignature(u);
			if (signature.contains("java.lang.String: boolean equals")) {
				return true;
			}
		}
		return false;
	}

	public static Expr addSensitive(PathCoverter theCoverter, Expr rightZ3) {
		System.out.println("	addSensitive ====== " + rightZ3);
		InterpolationContext ictx = theCoverter.getIctx();
		String oldname = "realString";
		if (!theCoverter.hasGlobal(oldname)) {
			Sort arraySort = ictx.mkArraySort(ictx.getIntSort(),
					ictx.getIntSort());
			Sort StringArraySort = ictx.mkArraySort(ictx.getIntSort(),
					arraySort);
			String globalName = theCoverter.getGlobalName(oldname);
			Expr realStringArray = ictx.mkConst(globalName, StringArraySort);
			theCoverter.addSubstitute(globalName, oldname);
			theCoverter.updateGlobal(oldname, realStringArray);
		}
		int StringSize = theCoverter.getRealArraySize(oldname);
		ArrayExpr theString = ictx.mkArrayConst("string" + StringSize + "path"
				+ theCoverter.getPathNumber(), ictx.getIntSort(),
				ictx.getIntSort());
		theCoverter.pushSensitiveInput(theString);
		ArrayExpr oldStringArray = (ArrayExpr) theCoverter.getGlobal(oldname);
		String newName = theCoverter.getGlobalName(oldname);
		ArrayExpr newStringArray = (ArrayExpr) ictx.mkConst(newName,
				oldStringArray.getSort());
		Expr afterStore = ictx.mkStore(oldStringArray, rightZ3, theString);
		BoolExpr newStringArrayEq = ictx.mkEq(newStringArray, afterStore);
		theCoverter.updateGlobal(oldname, newStringArray);
		theCoverter.addSubstitute(newName, oldname);
		return newStringArrayEq;
	}

	public static boolean ifNew(Unit u, Forest f1) {
		if (u instanceof AssignStmt) {
			AssignStmt aStmt = (AssignStmt) u;
			Value right = aStmt.getRightOp();
			if (right instanceof NewExpr) {
				if (f1.ifHasLoadFunction()) {
					return true;
				}
			}
		}

		return false;

	}

	public static boolean ifArrayCopy(Unit u) {
		if (HelpTree.isInvoke(u)) {
			String signature = HelpTree.getMethodSignature(u);
			if (signature.contains("java.lang.System: void arraycopy")) {
				return true;
			}
		}
		return false;
	}

	public static BoolExpr handleArrayCopy(InvokeStmt u, PathHelper theHelper,
			PathCoverter theCoverter) {
		InterpolationContext ictx = theCoverter.getIctx();
		InvokeExpr iExpr = u.getInvokeExpr();
		Value src = iExpr.getArg(0);
		Value srcStartv = iExpr.getArg(1);
		IntConstant srcStartI = (IntConstant) srcStartv;
		int srcStart = srcStartI.value;
		Value des = iExpr.getArg(2);
		Value desStartv = iExpr.getArg(3);
		IntConstant desStartI = (IntConstant) desStartv;
		int desStart = desStartI.value;
		Value lengthv = iExpr.getArg(4);
		IntConstant lengthi = (IntConstant) lengthv;
		int length = lengthi.value;
		Local src1 = (Local) src;
		Local des1 = (Local) src;
		ArrayExpr sourceArray = ArrayHelper.getRealArray(src1, theCoverter,
				theHelper);
		ArrayExpr desoldArray = ArrayHelper.getRealArray(des1, theCoverter,
				theHelper);
		Type t=src1.getType();
		String typeName=t.toString();
		String oldname = "realArray"+typeName;
		BoolExpr[] constrains=new BoolExpr[length];
		ArrayExpr theArray=null;
		if(length<1){
			return ictx.mkTrue();
		}
		for(int i=0;i<length;i++){
			int ArraySize = theCoverter.getRealArraySize(oldname);
			theArray = ictx.mkArrayConst("array" + ArraySize,
					ictx.getIntSort(), ictx.getIntSort());
			IntExpr srcIndex=ictx.mkInt(srcStart);
			IntExpr desIndex=ictx.mkInt(desStart);
			srcStart++;
			desStart++;
			Expr selectResult=ictx.mkSelect(sourceArray, srcIndex);
			Expr storeResult=ictx.mkStore(desoldArray, desIndex, selectResult);
			BoolExpr arrayEq=ictx.mkEq(theArray, storeResult);
			constrains[i]=arrayEq;
			desoldArray=theArray;
		}
		String realName = "realArray"+typeName;
		ArrayExpr realArray = (ArrayExpr) theCoverter.getGlobal(realName);
		String newName=theCoverter.getGlobalName(realName);
		ArrayExpr newGlobalArray=(ArrayExpr) ictx.mkConst(newName, realArray.getSort());
		Expr desExpr=theHelper.CovertValue(desStartv, false, 0);
		Expr oldStroe=ictx.mkStore(realArray, desExpr,theArray);
		BoolExpr newGlobalEq=ictx.mkEq(newGlobalArray, oldStroe);
		theCoverter.updateGlobal(realName, newGlobalArray);
		BoolExpr allConstrains=ictx.mkAnd(constrains);
		return ictx.mkAnd(allConstrains,newGlobalEq);
	}
}
