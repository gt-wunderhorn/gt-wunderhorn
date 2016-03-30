package z3_helper;

import java.util.Map;

import soot.Local;
import soot.Type;
import soot.Unit;
import soot.Value;
import soot.jimple.AnyNewExpr;
import soot.jimple.NewExpr;
import soot.jimple.StringConstant;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

public class StringHelper {
	static BoolExpr covertString(StringConstant s, PathCoverter theCoverter,
			Expr rightz3) {
		String a = s.value;
		InterpolationContext ictx = theCoverter.getIctx();
		BoolExpr[] result = new BoolExpr[a.length()];
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
		Sort arraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
		int StringSize = theCoverter.getRealArraySize(oldname);
		ArrayExpr theString = ictx.mkArrayConst("string" + StringSize,
				ictx.getIntSort(), ictx.getIntSort());
		for (int i = 0; i < a.length(); i++) {
			int theOne = a.charAt(i);
			IntExpr theOneZ3 = ictx.mkInt(theOne);
			IntExpr indexZ3 = ictx.mkInt(i);
			Expr select = ictx.mkSelect(theString, indexZ3);
			result[i] = ictx.mkEq(select, theOneZ3);
		}
		BoolExpr constrainsAboutArray = ictx.mkAnd(result);
		ArrayExpr oldStringArray = (ArrayExpr) theCoverter.getGlobal(oldname);
		String newName = theCoverter.getGlobalName(oldname);
		ArrayExpr newStringArray = (ArrayExpr) ictx.mkConst(newName,
				oldStringArray.getSort());
		Expr afterStore = ictx.mkStore(oldStringArray, rightz3, theString);
		BoolExpr newStringArrayEq = ictx.mkEq(newStringArray, afterStore);
		theCoverter.updateGlobal(oldname, newStringArray);
		theCoverter.addSubstitute(newName, oldname);
		return ictx.mkAnd(constrainsAboutArray, newStringArrayEq);
	}

	static boolean ifStringType(Type t) {
		String typeName = t.toString();
		if (typeName.equals("java.lang.String")) {
			return true;
		} else {
			return false;
		}
	}

	static Expr covertLocal(Local v, boolean IfAssignLeft,
			PathCoverter theCoverter, PathHelper theHelper, int i) {
		InterpolationContext ictx = theCoverter.getIctx();
		Local left1 = (Local) v;
		String oldName = left1.getName();
		// we need rename here
		if (IfAssignLeft) {
			//System.out.println("this is an array");
			String newName = oldName
					+ theCoverter.getRenameString(theHelper.getLevel())
					+ "index" + i;
			Sort arraySort = ictx.mkArraySort(ictx.mkIntSort(),
					ictx.mkIntSort());
			Expr leftExpr = theCoverter.getIctx().mkConst(newName, arraySort);
			theCoverter.addSubstitute(newName, oldName);
			theCoverter.updateSubstituteSort(newName, arraySort);
			if (theHelper.getLatestLocal().containsKey(oldName)) {
				theHelper.getLatestLocal().remove(oldName);
				theHelper.getLatestLocal().put(oldName, leftExpr);
			} else {
				theHelper.getLatestLocal().put(oldName, leftExpr);
			}
			return leftExpr;
		} else {
			return theHelper.getLatestLocal().get(oldName);
		}
	}

	public static Expr z3NewString(PathCoverter theCoverter, PathHelper PathHelper)
	{
		String TypeName ="StringType";
		Map<String, NewSort> sortId = theCoverter.getSortId();
		Map<String, Sort> newSortMap = theCoverter.getSort();
		if (sortId.containsKey(TypeName)) {
			NewSort s = sortId.get(TypeName);
			return s.getNewObject();
		} else {
			Sort newSort = null;
			if (newSortMap.containsKey(TypeName)) {
				newSort = newSortMap.get(TypeName);
			} else {
				newSort = theCoverter.getIctx().mkUninterpretedSort(TypeName);
				newSortMap.put(TypeName, newSort);
			}
			NewSort s = new NewSort(newSort, theCoverter);
			sortId.put(TypeName, s);
			return s.getNewObject();
		}
	}
}
