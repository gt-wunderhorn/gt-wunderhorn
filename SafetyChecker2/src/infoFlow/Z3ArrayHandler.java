package infoFlow;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

import soot.Local;
import soot.Type;
import soot.Value;
import soot.jimple.ArrayRef;

public class Z3ArrayHandler {

	public static Expr z3Local(Local local, boolean assignLeft, int nodeIndex, Z3ScriptHandler z3Handler) {
		InterpolationContext ictx = z3Handler.getIctx();
		Type type = local.getType();
		String typeString = type.toString();
		Sort newSort = null;
		
		if(z3Handler.getNewSortMap().containsKey(typeString)) {
			newSort = z3Handler.getNewSortMap().get(typeString);
		} else {
			newSort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
			z3Handler.getNewSortMap().put(typeString, newSort);
		}
		String sortName = typeString + z3Handler.getArraySortSuffix();
		String arrayName = typeString;

		if(!z3Handler.getLocalMap().containsKey(arrayName)) {
			Sort newArraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
			Expr newArray = ictx.mkConst(arrayName, newArraySort);
			z3Handler.getLocalMap().put(arrayName, newArray);
			NewSort ns = new NewSort(newSort, ictx);
			z3Handler.getSortId().put(sortName, ns);	
		}
		
		if(assignLeft) {
			String valueName = local.getName() + z3Handler.getArrayNameSuffix();
			Expr expr = ictx.mkConst(valueName, z3Handler.getNewSortMap().get(typeString));
			NewSort ns = z3Handler.getSortId().get(sortName);
			
			if(!ns.ifHasExpr(expr)) 
				ns.creatNewOject(expr);
			return expr;
		} else {
			ArrayExpr arrayExpr = (ArrayExpr) z3Handler.getLocalMap().get(arrayName);
			NewSort ns = z3Handler.getSortId().get(sortName);
			String valueName = local.getName() + z3Handler.getArrayNameSuffix(); 
			Expr expr = ictx.mkConst(valueName, z3Handler.getNewSortMap().get(typeString));

			Expr result = ictx.mkSelect(arrayExpr, ns.getId(expr));
			return result;
		}
	}

	public BoolExpr newArrayExpr(Expr rightZ3, Type type, Z3ScriptHandler z3Handler) {
		InterpolationContext ictx = z3Handler.getIctx();
		String typeName = type.toString();
		String oldName = this.getArrayPrefix() + typeName;

		if(!z3Handler.getGlobal().containsKey(oldName)) {
			Sort arraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
			Sort arrayCell = ictx.mkArraySort(ictx.getIntSort(), arraySort);

			String globalName = z3Handler.getGlobalName(oldName);  
			Expr arrayCellConst = ictx.mkConst(globalName, arrayCell);

			z3Handler.getSubstitute().put(globalName, oldName);
			z3Handler.getGlobal().put(oldName, arrayCellConst);
		}

		int arraySize = z3Handler.getRealArraySize(oldName);
		ArrayExpr arrayConst = ictx.mkArrayConst("array_" + arraySize, ictx.getIntSort(), ictx.getIntSort());
		ArrayExpr oldArray = (ArrayExpr) z3Handler.getGlobal().get(oldName);

		String newName = z3Handler.getGlobalName(oldName);
		ArrayExpr newArray = (ArrayExpr) ictx.mkConst(newName, oldArray.getSort());

		z3Handler.getGlobal().put(oldName, newArray);
		z3Handler.getSubstitute().put(newName, oldName);

		Expr afterStore = ictx.mkStore(oldArray, rightZ3, arrayConst);
		BoolExpr newArrayEq = ictx.mkEq(newArray, afterStore);
		return newArrayEq;
	}

	public Expr z3ArrayRef(ArrayRef value, Z3ScriptHandler z3Handler, Edge edge) {
		Value array = value.getBase(); 
		Value index = value.getIndex();
		Expr baseZ3 = z3Handler.convertValue(array, false, edge, edge.getSource().getDistance());

		IntExpr z3Index = (IntExpr) z3Handler.convertValue(index, false, edge, edge.getSource().getDistance());
		Type type = array.getType();
		String typeName = type.toString();
		String arrayName = this.getArrayPrefix() + typeName;
		ArrayExpr arrayExpr = (ArrayExpr) z3Handler.getGlobal().get(arrayName);
		ArrayExpr selectExpr = (ArrayExpr) z3Handler.getIctx().mkSelect(arrayExpr, baseZ3);
		Expr result = z3Handler.getIctx().mkSelect(selectExpr, z3Index);	

		return result;
	}

	public BoolExpr updateArrayRef(ArrayRef arrayRef, Z3ScriptHandler z3Handler, Expr rightZ3, Edge edge) {
		InterpolationContext ictx = z3Handler.getIctx();
		Value arrayBase = arrayRef.getBase();
		Value index = arrayRef.getIndex();

		Expr baseZ3 = z3Handler.convertValue(arrayBase, false, edge, edge.getSource().getDistance());
		IntExpr z3Index = (IntExpr) z3Handler.convertValue(index, false, edge, edge.getSource().getDistance());
		Type type = arrayBase.getType();
		String typeName = type.toString();
		String arrayName = getArrayPrefix() + typeName;
		ArrayExpr arrayExpr = (ArrayExpr) z3Handler.getGlobal().get(arrayName);
		ArrayExpr selectExpr = (ArrayExpr) ictx.mkSelect(arrayExpr, baseZ3);
		ArrayExpr storeExpr = ictx.mkStore(selectExpr, z3Index, rightZ3);

		int arraySize = z3Handler.getRealArraySize(arrayName);
		ArrayExpr newArray = ictx.mkArrayConst("array_" + arraySize, ictx.getIntSort(), ictx.getIntSort());
		BoolExpr newArrayEq = ictx.mkEq(newArray, storeExpr);
		ArrayExpr oldArray = (ArrayExpr) z3Handler.getGlobal().get(arrayName);
		String newName = z3Handler.getGlobalName(arrayName);
		ArrayExpr currentArray = (ArrayExpr) ictx.mkConst(newName, oldArray.getSort());
		Expr storeNewArray = ictx.mkStore(oldArray, baseZ3, newArray);
		BoolExpr currentEq = ictx.mkEq(currentArray, storeNewArray); 

		z3Handler.getGlobal().put(arrayName, currentArray);
		return ictx.mkAnd(newArrayEq, currentEq);
	}

	private String getArrayPrefix() {
		return "realArray_";
	}

}
