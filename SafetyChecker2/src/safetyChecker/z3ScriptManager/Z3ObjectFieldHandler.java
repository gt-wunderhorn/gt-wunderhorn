package safetyChecker.z3ScriptManager;

import java.util.Map;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

import safetyChecker.NewSort;

import soot.Local;
import soot.Type;

public class Z3ObjectFieldHandler {

	protected Expr handleStaticFieldRef(Local local, boolean assignLeft, Z3ScriptHandler z3Handler) {
		InterpolationContext ictx = z3Handler.getIctx();

		Type type = local.getType();
		String typeName = type.toString();
		Sort newSort = null;
		Map<String, Sort> newSortMap = z3Handler.getNewSortMap();
		Map<String, NewSort> sortId = z3Handler.getSortId();

		if(newSortMap.containsKey(typeName)) {
			newSort = newSortMap.get(typeName);
		} else {
			newSort = ictx.mkUninterpretedSort(ictx.mkSymbol(typeName));
			newSortMap.put(typeName, newSort);	
		}

		if(!z3Handler.getGlobal().containsKey(typeName)) {
			Sort newArraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
			String arrayName = z3Handler.getGlobalName(typeName);
			Expr newArray = ictx.mkConst(arrayName, newArraySort);
			z3Handler.getGlobal().put(typeName, newArray);
			NewSort ns = new NewSort(newSort, ictx);
			sortId.put(typeName, ns);
		}

		if(assignLeft) {
			String valueName = local.getName();
			Expr a = ictx.mkConst(valueName, newSortMap.get(typeName));
			NewSort s2 = sortId.get(typeName);
			if(s2.ifHasExpr(a)) {
				return a;
			}else {
				s2.creatNewOject(a);
				return a;
			}	
		} else {
			ArrayExpr oldArray = (ArrayExpr) z3Handler.getGlobal().get(typeName);
			NewSort s2 = sortId.get(typeName);
			String valueName = local.getName();
			Expr a = ictx.mkConst(valueName, newSortMap.get(typeName));
			Expr result = ictx.mkSelect(oldArray, s2.getId(a));
			return result;
		}
	}

}
