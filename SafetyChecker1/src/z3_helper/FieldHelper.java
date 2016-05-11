package z3_helper;

import infoFlow.Node;

import java.util.ArrayList;
import java.util.Map;

import soot.Local;
import soot.RefLikeType;
import soot.SootField;
import soot.Type;
import soot.Value;
import soot.jimple.FieldRef;
import soot.jimple.InstanceFieldRef;
import soot.jimple.StaticFieldRef;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

public class FieldHelper {
	public static Expr z3Object(InstanceFieldRef v, boolean IfAssignLeft,
			PathCoverter theCoverter, PathHelper pathHelper) {
		Value base = v.getBase();
		SootField Field = v.getField();
		Expr baseZ3 = pathHelper.CovertValue(base, false, 0);
		String FieldName = Field.toString();
		if (!theCoverter.hasGlobal(FieldName)) {
			Sort newArraySort = theCoverter.getIctx().mkArraySort(
					theCoverter.getIctx().getIntSort(),
					theCoverter.getIctx().getIntSort());
			String arrayName = theCoverter.getGlobalName(FieldName);
			Expr newArray = theCoverter.getIctx().mkConst(arrayName,
					newArraySort);
			theCoverter.updateGlobal(FieldName, newArray);
		}
		if (IfAssignLeft) {
			return baseZ3;
		} else {
			ArrayExpr oldArray = (ArrayExpr) theCoverter.getGlobal(FieldName);
			Expr result = theCoverter.getIctx().mkSelect(oldArray, baseZ3);
			return result;
		}
	}

	public static Expr staticField(StaticFieldRef v, boolean IfAssignLeft,
			PathCoverter theCoverter, PathHelper pathHelper) {
		return null;
	}
}
