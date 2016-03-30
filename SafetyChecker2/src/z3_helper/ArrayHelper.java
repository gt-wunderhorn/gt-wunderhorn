package z3_helper;

import infoFlow.Node;

import java.util.ArrayList;
import java.util.Map;

import soot.ArrayType;
import soot.Local;
import soot.PrimType;
import soot.RefLikeType;
import soot.RefType;
import soot.Type;
import soot.Value;
import soot.jimple.ArrayRef;
import soot.jimple.InstanceFieldRef;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

public class ArrayHelper {
	public static Expr z3Object(ArrayRef v, PathCoverter theCoverter,
			PathHelper pathHelper, Map<String, Expr> latestLocal) {
		Value oldArray = v.getBase();
		Value index = v.getIndex();
		Expr baseZ3 = pathHelper.CovertValue(oldArray, false, 0);
		IntExpr z3Index = (IntExpr) pathHelper.CovertValue(index, false, 0);
		Type t = oldArray.getType();
		String typeName = t.toString();
		String realName = "realArray"+typeName;
		ArrayExpr realArray = (ArrayExpr) theCoverter.getGlobal(realName);
		ArrayExpr resultArray = (ArrayExpr) theCoverter.getIctx().mkSelect(
				realArray, baseZ3);
		Expr result = theCoverter.getIctx().mkSelect(resultArray, z3Index);
		return result;
	}

	public static ArrayExpr getRealArray(Local v, PathCoverter theCoverter,
			PathHelper pathHelper) {
		Expr baseZ3 = pathHelper.CovertValue(v, false, 0);
		Type t = v.getType();
		String typeName = t.toString();
		String realName = "realArray"+typeName;
		ArrayExpr realArray = (ArrayExpr) theCoverter.getGlobal(realName);
		ArrayExpr resultArray = (ArrayExpr) theCoverter.getIctx().mkSelect(
				realArray, baseZ3);
		return resultArray;
	}

	public static BoolExpr updateArrayRef(ArrayRef v, PathCoverter theCoverter,
			PathHelper pathHelper, Expr rightZ3) {
		InterpolationContext ictx = theCoverter.getIctx();
		Value oldArray = v.getBase();
		Value index = v.getIndex();
		Expr baseZ3 = pathHelper.CovertValue(oldArray, false, 0);
		IntExpr z3Index = (IntExpr) pathHelper.CovertValue(index, false, 0);
		Type t = oldArray.getType();
		String typeName = t.toString();
		String realName = "realArray"+typeName;
		ArrayExpr realArray = (ArrayExpr) theCoverter.getGlobal(realName);
		ArrayExpr resultArray = (ArrayExpr) theCoverter.getIctx().mkSelect(
				realArray, baseZ3);
		Expr storeInoldResultArray = ictx
				.mkStore(resultArray, z3Index, rightZ3);
		Sort arraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
		int ArraySize = theCoverter.getRealArraySize(realName);
		ArrayExpr newResultArray = ictx.mkArrayConst("array" + ArraySize
				+ "Path" + theCoverter.getPathNumber(), ictx.getIntSort(),
				ictx.getIntSort());
		BoolExpr newResultArrayEq = ictx.mkEq(newResultArray,
				storeInoldResultArray);
		ArrayExpr oldRealArray = (ArrayExpr) theCoverter.getGlobal(realName);
		String newName = theCoverter.getGlobalName(realName);
		ArrayExpr newRealArray = (ArrayExpr) ictx.mkConst(newName,
				oldRealArray.getSort());
		Expr storeNewArrayInRealArray = ictx.mkStore(oldRealArray, baseZ3,
				newResultArray);
		BoolExpr newRealArrayEq = ictx.mkEq(newRealArray,
				storeNewArrayInRealArray);
		theCoverter.updateGlobal(realName, newRealArray);
		return ictx.mkAnd(newResultArrayEq, newRealArrayEq);

	}

	public static Expr z3Local(Local v, boolean IfAssignLeft,
			PathCoverter theCoverter, int nodeIndex,
			Map<String, Expr> latestLocal, int level) {
		InterpolationContext ictx = theCoverter.getIctx();
		Type t = v.getType();
		String TypeName = t.toString();
		Sort newSort = null;
		Map<String, Sort> newSortMap = theCoverter.getSort();
		Map<String, NewSort> sortId = theCoverter.getSortId();
		if (newSortMap.containsKey(TypeName)) {
			newSort = newSortMap.get(TypeName);
		} else {
			newSort = theCoverter.getIctx().mkArraySort(ictx.getIntSort(),
					ictx.getIntSort());
			newSortMap.put(TypeName, newSort);
		}
		String sortName = TypeName + "virtual" + "level" + level;
		String arrayName = TypeName + "Path" + theCoverter.getPathNumber()
				+ "level" + level;
		if (!latestLocal.containsKey(arrayName)) {
			Sort newArraySort = theCoverter.getIctx().mkArraySort(
					theCoverter.getIctx().getIntSort(),
					theCoverter.getIctx().getIntSort());
			Expr newArray = theCoverter.getIctx().mkConst(arrayName,
					newArraySort);
			latestLocal.put(arrayName, newArray);
			NewSort s = new NewSort(newSort, theCoverter);
			sortId.put(sortName, s);
		}
		if (IfAssignLeft) {
			String valueName = v.getName() + theCoverter.getRenameString(level);
			Expr a = theCoverter.getIctx().mkConst(valueName,
					newSortMap.get(TypeName));
			NewSort s2 = sortId.get(sortName);
			if (s2.ifHasExpr(a)) {
				return a;
			} else {
				s2.creatNewOject(a);
				return a;
			}
		} else {
			ArrayExpr virtualArray = (ArrayExpr) latestLocal.get(arrayName);
			NewSort s = sortId.get(sortName);
			String valueName = v.getName() + theCoverter.getRenameString(level);
			Expr value = theCoverter.getIctx().mkConst(valueName,
					newSortMap.get(TypeName));
			Expr result = theCoverter.getIctx().mkSelect(virtualArray,
					s.getId(value));
			return result;

		}
	}

	public static Sort CovertType(Type e, InterpolationContext iCtx,
			Map<String, Sort> newSortMap) {
		if (e instanceof PrimType) {
			return iCtx.getIntSort();
		} else {
			String TypeName = e.toString();
			if (e instanceof RefType) {
				Sort newSort = iCtx
						.mkUninterpretedSort(iCtx.mkSymbol(TypeName));
				newSortMap.put(TypeName, newSort);
				return iCtx.getIntSort();
			} else {
				ArrayType a = (ArrayType) e;
				Type elementType = a.getElementType();
				Sort elementSort = CovertType(elementType, iCtx, newSortMap);
				Sort arraySort = iCtx.mkArraySort(iCtx.getIntSort(),
						elementSort);
				newSortMap.put(TypeName, arraySort);
				return arraySort;

			}
		}
	}
}
