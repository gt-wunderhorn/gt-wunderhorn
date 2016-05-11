package z3_helper;

import java.util.Map;

import soot.Type;
import soot.Value;
import soot.jimple.AnyNewExpr;
import soot.jimple.IntConstant;
import soot.jimple.NewArrayExpr;
import soot.jimple.NewExpr;
import soot.jimple.NewMultiArrayExpr;
import soot.jimple.StringConstant;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

public class NewExprHelper {
	public static BoolExpr spaceFormula(Value right, PathCoverter theCoverter,
			Map<String, Expr> latestLocal, PathHelper PathHelper, Expr rightZ3) {
		if (right instanceof NewExpr) {
			return theCoverter.getIctx().mkTrue();
		}
		if (right instanceof NewArrayExpr) {
			return realArray(theCoverter, rightZ3,right.getType());
		}
		if (right instanceof NewMultiArrayExpr){
			NewMultiArrayExpr nMExpr=(NewMultiArrayExpr) right;
			return multiArray(nMExpr,theCoverter,rightZ3);
		}
		return null;
	}
	static private BoolExpr multiArray(NewMultiArrayExpr v,PathCoverter theCoverter,Expr rightZ3){
		InterpolationContext ictx = theCoverter.getIctx();
		IntConstant sizeI=(IntConstant) v.getSize(0);
		int size=sizeI.value;
		size++;
		Type t=v.getBaseType().getElementType();
		Map<String, NewSort> sortId = theCoverter.getSortId();
		Map<String, Sort> newSortMap = theCoverter.getSort();
		Expr[] LowrightZ3=new Expr[size];
		BoolExpr[] constrains=new BoolExpr[size];
		String virtualName =t.toString();
		NewSort s=null;
		if (sortId.containsKey(virtualName)) {
			s = sortId.get(virtualName);
		} else {
			Sort newSort = null;
			if (newSortMap.containsKey(virtualName)) {
				newSort = newSortMap.get(virtualName);
			} else {
				newSort = theCoverter.getIctx().mkUninterpretedSort(
						virtualName);
				newSortMap.put(virtualName, newSort);
			}
			s = new NewSort(newSort, theCoverter);
			sortId.put(virtualName, s);
		}
		for(int i=0;i<size;i++){
			LowrightZ3[i]=s.getNewObject();
			constrains[i]=realArray(theCoverter,LowrightZ3[i],t);
		}
		Type t1=v.getType();
		String typeName=t1.toString();
		String oldname = "realArray"+typeName;
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
		int ArraySize = theCoverter.getRealArraySize(oldname);
		ArrayExpr theArray = ictx.mkArrayConst("array" + ArraySize+"Path"+theCoverter.getPathNumber(),
				ictx.getIntSort(), ictx.getIntSort());
		BoolExpr[] theArrayConstrains=new BoolExpr[size];
		for(int i=0;i<size;i++){
			Expr select=ictx.mkSelect(theArray, ictx.mkInt(i));
			theArrayConstrains[i]=ictx.mkEq(select,LowrightZ3[i]);
		}
		ArrayExpr oldRealArray = (ArrayExpr) theCoverter.getGlobal(oldname);
		String newName = theCoverter.getGlobalName(oldname);
		ArrayExpr newRealArray = (ArrayExpr) ictx.mkConst(newName,
				oldRealArray.getSort());
		Expr afterStore = ictx.mkStore(oldRealArray, rightZ3, theArray);
		BoolExpr newRealArrayEq = ictx.mkEq(newRealArray, afterStore);
		theCoverter.updateGlobal(oldname, newRealArray);
		theCoverter.addSubstitute(newName, oldname);
		BoolExpr allLowConstrains=ictx.mkAnd(constrains);
		BoolExpr thisArrayConstrains=ictx.mkAnd(theArrayConstrains);
		BoolExpr all=ictx.mkAnd(allLowConstrains,thisArrayConstrains,newRealArrayEq);
		return all;
	}
	static private BoolExpr realArray(PathCoverter theCoverter, Expr rightz3,Type t) {
		InterpolationContext ictx = theCoverter.getIctx();
		String typeName=t.toString();
		String oldname = "realArray"+typeName;
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
		int ArraySize = theCoverter.getRealArraySize(oldname);
		ArrayExpr theArray = ictx.mkArrayConst("array" + ArraySize+"Path"+theCoverter.getPathNumber(),
				ictx.getIntSort(), ictx.getIntSort());
		ArrayExpr oldRealArray = (ArrayExpr) theCoverter.getGlobal(oldname);
		String newName = theCoverter.getGlobalName(oldname);
		ArrayExpr newRealArray = (ArrayExpr) ictx.mkConst(newName,
				oldRealArray.getSort());
		Expr afterStore = ictx.mkStore(oldRealArray, rightz3, theArray);
		BoolExpr newRealArrayEq = ictx.mkEq(newRealArray, afterStore);
		theCoverter.updateGlobal(oldname, newRealArray);
		theCoverter.addSubstitute(newName, oldname);
		return newRealArrayEq;
	}

	public static Expr z3NewExpr(AnyNewExpr v, PathCoverter theCoverter,
			Map<String, Expr> latestLocal, PathHelper PathHelper) {
		Type t = v.getType();
		String TypeName = t.toString();
		Map<String, NewSort> sortId = theCoverter.getSortId();
		Map<String, Sort> newSortMap = theCoverter.getSort();
		if (v instanceof NewExpr) {
			NewExpr l1 = (NewExpr) v;
			if (sortId.containsKey(TypeName)) {
				NewSort s = sortId.get(TypeName);
				return s.getNewObject();
			} else {
				Sort newSort = null;
				if (newSortMap.containsKey(TypeName)) {
					newSort = newSortMap.get(TypeName);
				} else {
					newSort = theCoverter.getIctx().mkUninterpretedSort(
							TypeName);
					newSortMap.put(TypeName, newSort);
				}
				NewSort s = new NewSort(newSort, theCoverter);
				sortId.put(TypeName, s);
				return s.getNewObject();
			}
		}
		if (v instanceof NewArrayExpr) {
			String virtualName = TypeName;
			if (sortId.containsKey(virtualName)) {
				NewSort s = sortId.get(virtualName);
				return s.getNewObject();
			} else {
				Sort newSort = null;
				if (newSortMap.containsKey(virtualName)) {
					newSort = newSortMap.get(virtualName);
				} else {
					newSort = theCoverter.getIctx().mkUninterpretedSort(
							virtualName);
					newSortMap.put(virtualName, newSort);
				}
				NewSort s = new NewSort(newSort, theCoverter);
				sortId.put(virtualName, s);
				return s.getNewObject();
			}
		}
		if (v instanceof NewMultiArrayExpr) {
			String virtualName = TypeName;
			if (sortId.containsKey(virtualName)) {
				NewSort s = sortId.get(virtualName);
				return s.getNewObject();
			} else {
				Sort newSort = null;
				if (newSortMap.containsKey(virtualName)) {
					newSort = newSortMap.get(virtualName);
				} else {
					newSort = theCoverter.getIctx().mkUninterpretedSort(
							virtualName);
					newSortMap.put(virtualName, newSort);
				}
				NewSort s = new NewSort(newSort, theCoverter);
				sortId.put(virtualName, s);
				return s.getNewObject();
			}
		}
		return null;
	}
}
