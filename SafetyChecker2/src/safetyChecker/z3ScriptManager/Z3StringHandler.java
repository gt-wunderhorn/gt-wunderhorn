package safetyChecker.z3ScriptManager;

import java.util.Map;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

import safetyChecker.NewSort;
import safetyChecker.utilities.LogUtils;

import soot.Local;
import soot.Type;
import soot.jimple.StringConstant;

public class Z3StringHandler {
	static BoolExpr covertString(StringConstant s, Z3ScriptHandler z3Handler, Expr rightz3) {
		String a = (s != null) ? s.value : "";
		InterpolationContext ictx = z3Handler.getIctx();
		BoolExpr[] result = new BoolExpr[a.length()];
		String oldname = "realString";
		if (!z3Handler.getGlobal().containsKey(oldname)) {
			Sort arraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
			Sort StringArraySort = ictx.mkArraySort(ictx.getIntSort(), arraySort);
			String globalName = z3Handler.getGlobalName(oldname);
			Expr realStringArray = ictx.mkConst(globalName, StringArraySort);
			z3Handler.getSubstitute().put(globalName, oldname);
			z3Handler.getGlobal().put(oldname, realStringArray);
		}
		Sort arraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
		int StringSize = z3Handler.getRealArraySize(oldname);
		ArrayExpr theString = ictx.mkArrayConst("string" + StringSize, ictx.getIntSort(), ictx.getIntSort());
		for (int i = 0; i < a.length(); i++) {
			int theOne = a.charAt(i);
			IntExpr theOneZ3 = ictx.mkInt(theOne);
			IntExpr indexZ3 = ictx.mkInt(i);
			Expr select = ictx.mkSelect(theString, indexZ3);
			result[i] = ictx.mkEq(select, theOneZ3);
		}
		BoolExpr constrainsAboutArray = ictx.mkAnd(result);
		ArrayExpr oldStringArray = (ArrayExpr) z3Handler.getGlobal().get(oldname);
		String newName = z3Handler.getGlobalName(oldname);
		ArrayExpr newStringArray = (ArrayExpr) ictx.mkConst(newName, oldStringArray.getSort());
		Expr afterStore = ictx.mkStore(oldStringArray, rightz3, theString);
		BoolExpr newStringArrayEq = ictx.mkEq(newStringArray, afterStore);
		z3Handler.getGlobal().put(oldname, newStringArray);
		z3Handler.getSubstitute().put(newName, oldname);
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

	public static Expr z3NewString(Z3ScriptHandler z3Handler) {
		String TypeName = "StringType";
		Map<String, NewSort> sortId = z3Handler.getSortId();
		Map<String, Sort> newSortMap = z3Handler.getNewSortMap();
		if (sortId.containsKey(TypeName)) {
			NewSort s = sortId.get(TypeName);
			return s.getNewObject();
		} else {
			Sort newSort = null;
			if (newSortMap.containsKey(TypeName)) {
				newSort = newSortMap.get(TypeName);
			} else {
				newSort = z3Handler.getIctx().mkUninterpretedSort(TypeName);
				newSortMap.put(TypeName, newSort);
			}
			NewSort s = new NewSort(newSort, z3Handler.getIctx());
			sortId.put(TypeName, s);
			return s.getNewObject();
		}
	}
}
