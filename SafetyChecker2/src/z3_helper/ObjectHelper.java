package z3_helper;

import infoFlow.Node;

import java.util.ArrayList;
import java.util.Map;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

import soot.Local;
import soot.RefType;
import soot.Type;
import soot.Value;

public class ObjectHelper {
	public static Expr z3Object(Local v, boolean IfAssignLeft,
			PathCoverter theCoverter, int level) {
		Type t = v.getType();
		String TypeName = t.toString();
		Sort newSort = null;
		Map<String, Sort> newSortMap = theCoverter.getSort();
		Map<String, NewSort> sortId = theCoverter.getSortId();
		if (newSortMap.containsKey(TypeName)) {
			newSort = newSortMap.get(TypeName);
		} else {
			newSort = theCoverter.getIctx().mkUninterpretedSort(
					theCoverter.getIctx().mkSymbol(TypeName));
			newSortMap.put(TypeName, newSort);
		}
		if (!theCoverter.hasGlobal(TypeName)) {
			Sort newArraySort = theCoverter.getIctx().mkArraySort(
					theCoverter.getIctx().getIntSort(),
					theCoverter.getIctx().getIntSort());
			String arrayName = theCoverter.getGlobalName(TypeName);
			Expr newArray = theCoverter.getIctx().mkConst(arrayName,
					newArraySort);
			theCoverter.updateGlobal(TypeName, newArray);
			theCoverter.addSubstitute(TypeName, arrayName);
			NewSort s = new NewSort(newSort, theCoverter);
			sortId.put(TypeName, s);
		}
		if (IfAssignLeft) {
			String valueName = v.getName() + theCoverter.getRenameString(level);
			Expr a = theCoverter.getIctx().mkConst(valueName,
					newSortMap.get(TypeName));
			NewSort s2 = sortId.get(TypeName);
			if (s2.ifHasExpr(a)) {
				return a;
			} else {
				s2.creatNewOject(a);
				return a;
			}
		} else {
			ArrayExpr oldArray = (ArrayExpr) theCoverter.getGlobal(TypeName);
			NewSort s2 = sortId.get(TypeName);
			String valueName = v.getName() + theCoverter.getRenameString(level);
			Expr a = theCoverter.getIctx().mkConst(valueName,
					newSortMap.get(TypeName));
			//System.out.println(v);
			Expr result = theCoverter.getIctx().mkSelect(oldArray, s2.getId(a));
			return result;
		}

	}
}
