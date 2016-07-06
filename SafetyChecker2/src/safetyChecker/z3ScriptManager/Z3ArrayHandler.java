package safetyChecker.z3ScriptManager;

import java.util.Map;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

import safetyChecker.Edge;
import safetyChecker.NewSort;
import safetyChecker.utilities.LogUtils;

import soot.Local;
import soot.Type;
import soot.Value;
import soot.jimple.ArrayRef;
import soot.jimple.IntConstant;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;
import soot.jimple.NewArrayExpr;
import soot.jimple.NewMultiArrayExpr;

public class Z3ArrayHandler {
	// arg indexes for System.arraycopy
	private int argSrcIndex = 0;
	private int argSrcStartIndex = 1;
	private int argDtcIndex = 2;
	private int argDtcStartIndex = 3;
	private int argLengthIndex = 4;
	// arg indexes for Arrays.equals
	private int argArray1 = 0;
	private int argArray2 = 1;

	public Expr z3Local(Local local, boolean assignLeft, int nodeIndex, Z3ScriptHandler z3Handler) {
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

	protected Expr convertNewArrayExpr(NewArrayExpr ne, Edge e, Z3ScriptHandler z3Handler) {
		Type type = ne.getType();
		String virtualName = type.toString();
		Map<String, NewSort> sortId = z3Handler.getSortId();	
		Map<String, Sort> newSortMap =  z3Handler.getNewSortMap();
		InterpolationContext ictx = z3Handler.getIctx();

		if(sortId.containsKey(virtualName)) {
			NewSort ns = sortId.get(virtualName);
			return ns.getNewObject();
		} else {
			Sort newSort = null;
			if(newSortMap.containsKey(virtualName)) {
				newSort = newSortMap.get(virtualName);
			} else {
				newSort = ictx.mkUninterpretedSort(virtualName);
				newSortMap.put(virtualName, newSort);
			}
			NewSort ns = new NewSort(newSort, ictx);
			sortId.put(virtualName, ns);
			return ns.getNewObject();
		}
	}
	
	protected Expr convertNewMultiArrayExpr(NewMultiArrayExpr ne, Edge e, Z3ScriptHandler z3Handler) {
		Type type = ne.getType();
		String virtualName = type.toString();
		Map<String, NewSort> sortId = z3Handler.getSortId();	
		Map<String, Sort> newSortMap =  z3Handler.getNewSortMap();
		InterpolationContext ictx = z3Handler.getIctx();
		
		if(sortId.containsKey(virtualName)) {
			NewSort ns = sortId.get(virtualName);
			return ns.getNewObject();
		} else {
			Sort newSort = null;
			if(newSortMap.containsKey(virtualName)) {
				newSort = newSortMap.get(virtualName);
			} else {
				newSort = ictx.mkUninterpretedSort(virtualName);
				newSortMap.put(virtualName, newSort);
			}
			NewSort ns = new NewSort(newSort, ictx);
			sortId.put(virtualName, ns);
			return ns.getNewObject();
		}
	}

	public BoolExpr newArrayExpr(Expr rightZ3, Type type, Z3ScriptHandler z3Handler) {
		InterpolationContext ictx = z3Handler.getIctx();
		String typeName = type.toString().substring(0, type.toString().indexOf("]")+1);
		String oldName = this.getArrayPrefix() + typeName;

		if(!z3Handler.getGlobal().containsKey(oldName)) {
			Sort arraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
			Sort arrayCell = ictx.getIntSort();
			Sort arrayMem = ictx.mkArraySort(arrayCell, arraySort);

			String globalName = z3Handler.getGlobalName(oldName);  
			Expr arrayCellConst = ictx.mkConst(globalName, arrayMem);
			LogUtils.debugln("arrayCellConst=" + arrayCellConst);

			z3Handler.getSubstitute().put(globalName, oldName);
			z3Handler.getGlobal().put(oldName, arrayCellConst);
		}

		int arraySize = z3Handler.getRealArraySize(oldName);
		ArrayExpr arrayConst = ictx.mkConstArray(ictx.getIntSort(), ictx.mkInt(0));
		ArrayExpr oldArray = (ArrayExpr) z3Handler.getGlobal().get(oldName);

		String newName = z3Handler.getGlobalName(oldName);
		ArrayExpr newArray = (ArrayExpr) ictx.mkConst(newName, oldArray.getSort());

		z3Handler.getGlobal().put(oldName, newArray);
		z3Handler.getSubstitute().put(newName, oldName);

		Expr afterStore = ictx.mkStore(oldArray, rightZ3, arrayConst);
		BoolExpr newArrayEq = ictx.mkEq(newArray, afterStore);
		  
		return newArrayEq;
	}


	public BoolExpr newMultiArrayExpr(NewMultiArrayExpr nmae, Type type, Z3ScriptHandler z3Handler, Expr rightZ3) {
		InterpolationContext ictx = z3Handler.getIctx();
		IntConstant sizeI=(IntConstant) nmae.getSize(0);
		int size=sizeI.value;
		size++;
		Type t=nmae.getBaseType().getElementType();
		Map<String, NewSort> sortId = z3Handler.getSortId();
		Map<String, Sort> newSortMap = z3Handler.getNewSortMap();
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
				newSort = ictx.mkUninterpretedSort(virtualName);
				newSortMap.put(virtualName, newSort);
			}
			s = new NewSort(newSort, ictx);
			sortId.put(virtualName, s);
		}
		for(int i=0;i<size;i++){
			LowrightZ3[i]=s.getNewObject();
			constrains[i]=this.newArrayExpr(LowrightZ3[i],t, z3Handler);
		}
		Type t1=nmae.getType();
		String typeName=t1.toString();
		String oldname = this.getArrayPrefix()+typeName;
		if (!z3Handler.getGlobal().containsKey(oldname)) {
			Sort arraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
			Sort StringArraySort = ictx.mkArraySort(ictx.getIntSort(), arraySort);
			String globalName = z3Handler.getGlobalName(oldname);
			Expr realStringArray = ictx.mkConst(globalName, StringArraySort);
			z3Handler.getSubstitute().put(globalName, oldname);
			z3Handler.getGlobal().put(oldname, realStringArray);
		}
		Sort arraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
		int ArraySize = z3Handler.getRealArraySize(oldname);
		ArrayExpr theArray = ictx.mkArrayConst("array_" + ArraySize,ictx.getIntSort(), ictx.getIntSort());
		BoolExpr[] theArrayConstrains=new BoolExpr[size];
		for(int i=0;i<size;i++){
			Expr select=ictx.mkSelect(theArray, ictx.mkInt(i));
			theArrayConstrains[i]=ictx.mkEq(select,LowrightZ3[i]);
		}
		ArrayExpr oldRealArray = (ArrayExpr) z3Handler.getGlobal().get(oldname);
		String newName = z3Handler.getGlobalName(oldname);
		ArrayExpr newRealArray = (ArrayExpr) ictx.mkConst(newName,oldRealArray.getSort());
		Expr afterStore = ictx.mkStore(oldRealArray, rightZ3, theArray);
		BoolExpr newRealArrayEq = ictx.mkEq(newRealArray, afterStore);
		z3Handler.getGlobal().put(oldname, newRealArray);
		z3Handler.getSubstitute().put(newName, oldname);
		BoolExpr allLowConstrains=ictx.mkAnd(constrains);
		BoolExpr thisArrayConstrains=ictx.mkAnd(theArrayConstrains);
		BoolExpr all=ictx.mkAnd(allLowConstrains,thisArrayConstrains,newRealArrayEq);
		return all;
	}

	public BoolExpr newMultiArrayExpr2(NewMultiArrayExpr nmae, Type type, Z3ScriptHandler z3Handler, Expr rightZ3) {
//		BoolExpr aa = this.newMultiArrayExpr2(nmae, type, z3Handler, rightZ3);
//		if(aa!=null) return aa;
		InterpolationContext ictx = z3Handler.getIctx();
//		int dimensionNumber = nmae.getSizeCount();
//		IntConstant sizeI = (IntConstant) nmae.getSize(0); 	
//		int size = 0;//sizeI.value;
//		size++;

		Type t=nmae.getType();
		Map<String, NewSort> sortId = z3Handler.getSortId();
		Map<String, Sort> newSortMap = z3Handler.getNewSortMap();

//		Expr[] LowrightZ3 = new Expr[size];
//		BoolExpr[] constraints = new BoolExpr[size];
		String virtualName = t.toString();
		virtualName = type.toString().substring(0, type.toString().indexOf("]")+1);

		NewSort s = null;
		if(sortId.containsKey(virtualName)) {
			s = sortId.get(virtualName);
		} else {
			Sort newSort = null;
			if(newSortMap.containsKey(virtualName)) {
				newSort = newSortMap.get(virtualName);
			} else {
				newSort = ictx.mkUninterpretedSort(virtualName);
				newSortMap.put(virtualName, newSort);	
			}
			s = new NewSort(newSort, ictx);
			sortId.put(virtualName, s);
		}

//		for(int i = 0; i < size; i++) {
//			LowrightZ3[i] = s.getNewObject();;
//			constraints[i] = newArrayExpr(LowrightZ3[i], t, z3Handler); 
//		}
		Expr firstDimObj = s.getNewObject();
		BoolExpr firtDimArray = this.newArrayExpr(firstDimObj, t, z3Handler);

		Type t1 = nmae.getType();
		String typeName = t1.toString();
		String oldName = this.getArrayPrefix() + typeName;
		if(!z3Handler.getGlobal().containsKey(oldName)) {
			Sort arraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
			Sort stringArraySort = ictx.mkArraySort(ictx.getIntSort(), arraySort);
			String globalName = z3Handler.getGlobalName(oldName);
			Expr realStringArray = ictx.mkConst(globalName, stringArraySort);
			z3Handler.getSubstitute().put(globalName, oldName);
			z3Handler.getGlobal().put(oldName, realStringArray);
		}

//*		Sort arraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
		int ArraySize = z3Handler.getRealArraySize(oldName);
		ArrayExpr theArray = ictx.mkArrayConst("array_" + ArraySize, ictx.getIntSort(), ictx.getIntSort());
//***		ArrayExpr arrayConst = ictx.mkConstArray(ictx.getIntSort(), ictx.mkInt(0));

		BoolExpr theArrayConstrains = null;
			Expr select=ictx.mkSelect(theArray, ictx.mkInt(0));
			theArrayConstrains=ictx.mkEq(select,firstDimObj);

			Expr secondDimObj = s.getNewObject();
			Expr select2 = ictx.mkSelect(theArray, ictx.mkInt(1));
			BoolExpr curCellConstraint = ictx.mkEq(select2, secondDimObj);

		ArrayExpr oldRealArray = (ArrayExpr) z3Handler.getGlobal().get(oldName);
		String newName = z3Handler.getGlobalName(oldName);
		ArrayExpr newRealArray = (ArrayExpr) ictx.mkConst(newName, oldRealArray.getSort());
		Expr afterStore = ictx.mkStore(oldRealArray, rightZ3, theArray);
		BoolExpr newRealArrayEq = ictx.mkEq(newRealArray, afterStore);
		z3Handler.getGlobal().put(oldName, newRealArray);
		z3Handler.getSubstitute().put(newName, oldName);
		BoolExpr allLowConstrains=ictx.mkAnd(firtDimArray);
		BoolExpr thisArrayConstrains=ictx.mkAnd(theArrayConstrains);
		BoolExpr thisArrayConstrains2 = ictx.mkAnd(curCellConstraint);

		 

		BoolExpr all=ictx.mkAnd(allLowConstrains,thisArrayConstrains,newRealArrayEq, thisArrayConstrains2);
		LogUtils.debugln(all);
		return all;
	}

	public Expr z3ArrayRef(ArrayRef value, Z3ScriptHandler z3Handler, Edge edge) {
		Value array = value.getBase(); 
		Value index = value.getIndex();
		Expr baseZ3 = z3Handler.convertValue(array, false, edge, edge.getSource().getDistance());

		IntExpr z3Index = (IntExpr) z3Handler.convertValue(index, false, edge, edge.getSource().getDistance());
		Type type = array.getType();
		String typeName = type.toString();
		LogUtils.debugln(typeName);
		String arrayName = this.getArrayPrefix() + typeName;
		ArrayExpr arrayExpr = (ArrayExpr) z3Handler.getGlobal().get(arrayName);
		ArrayExpr selectExpr = (ArrayExpr) z3Handler.getIctx().mkSelect(arrayExpr, baseZ3);
		Expr result = z3Handler.getIctx().mkSelect(selectExpr, z3Index);	

		return result;
	}

	private Expr z3ArrayRef(Value array, Z3ScriptHandler z3Handler, Edge edge, int index) {
		Expr baseZ3 = z3Handler.convertValue(array, false, edge, edge.getSource().getDistance());

		IntExpr z3Index = (IntExpr) z3Handler.getIctx().mkInt(index);
		Type type = array.getType();
		String typeName = type.toString();
		String arrayName = this.getArrayPrefix() + typeName;
		ArrayExpr arrayExpr = (ArrayExpr) z3Handler.getGlobal().get(arrayName);
		ArrayExpr selectExpr = (ArrayExpr) z3Handler.getIctx().mkSelect(arrayExpr, baseZ3);
		Expr result = z3Handler.getIctx().mkSelect(selectExpr, z3Index);	

		return result;
	}

	private BoolExpr updateArrayRef(Value arrayBase, Z3ScriptHandler z3Handler, Expr rightZ3, Edge edge, int index) {
		InterpolationContext ictx = z3Handler.getIctx();

		Expr baseZ3 = z3Handler.convertValue(arrayBase, false, edge, edge.getSource().getDistance());
		IntExpr z3Index = (IntExpr) z3Handler.getIctx().mkInt(index);
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

	public BoolExpr z3ArrayCopy(Edge edge, Z3ScriptHandler z3Handler) {
		InterpolationContext ictx = z3Handler.getIctx();
		InvokeStmt iStmt = (InvokeStmt) edge.getUnit(); 
		InvokeExpr iExpr = iStmt.getInvokeExpr();
		
		Value srcValue = iExpr.getArg(this.argSrcIndex);
		Value srcStartValue = iExpr.getArg(this.argSrcStartIndex);
		IntConstant srcStartIC = (IntConstant) srcStartValue;
		int srcStart = srcStartIC.value;

		Value dtcValue = iExpr.getArg(this.argDtcIndex);
		Value dtcStartValue = iExpr.getArg(this.argDtcStartIndex);
		IntConstant dtcStartIC = (IntConstant) dtcStartValue;
		int dtcStart = dtcStartIC.value;

		Value lengthValue = iExpr.getArg(this.argLengthIndex);
		IntConstant lengthIC = (IntConstant) lengthValue;
		int length = lengthIC.value;
		if(length <= 0) 
			return ictx.mkTrue(); 

		Local srcLocal = (Local) srcValue;
		Local dtcLocal = (Local) dtcValue;

		ArrayExpr srcArray = this.getRealArray(srcLocal, edge, z3Handler); 

		Type type = srcLocal.getType();
		String typeName = type.toString();
		String oldName = this.getArrayPrefix() + typeName;


		BoolExpr[] constraints = new BoolExpr[length];
		ArrayExpr arrayExpr = null;
		for(int i = 0; i < length; i++) {
			ArrayExpr dtcArray = this.getRealArray(dtcLocal, edge, z3Handler); 
			int arraySize = z3Handler.getRealArraySize(oldName);
			arrayExpr = ictx.mkArrayConst("array_" + arraySize, ictx.getIntSort(), ictx.getIntSort());
			IntExpr srcIndex = ictx.mkInt(srcStart);
			IntExpr dtcIndex = ictx.mkInt(dtcStart);
			srcStart++;
			dtcStart++;

			Expr selectExpr = ictx.mkSelect(srcArray, srcIndex);
			Expr storeExpr = ictx.mkStore(dtcArray, dtcIndex, selectExpr);
			BoolExpr arrayEq = ictx.mkEq(arrayExpr, storeExpr);
			LogUtils.debugln("dtcArray=" + dtcArray);

			String realName = this.getArrayPrefix() + typeName;
			ArrayExpr realArray = (ArrayExpr) z3Handler.getGlobal().get(realName);
			String newName = z3Handler.getGlobalName(realName);
			LogUtils.debugln(">>>>>>>>>>>>>NewName=" + newName);
			ArrayExpr newGlobalArray = (ArrayExpr) ictx.mkConst(newName, realArray.getSort());
			Expr dtcExpr = z3Handler.convertValue(dtcLocal, false, edge, edge.getSource().getDistance());
			Expr oldStore = ictx.mkStore(realArray, dtcExpr, arrayExpr);
			BoolExpr newGlobalEq = ictx.mkEq(newGlobalArray, oldStore);
			z3Handler.getGlobal().put(realName, newGlobalArray);
			constraints[i] = ictx.mkAnd(arrayEq, newGlobalEq);
			LogUtils.debugln(constraints[i]);
		}

		BoolExpr allConstraints = ictx.mkAnd(constraints);
		return allConstraints;
	}
	  
	public Expr z3ArraysEqual(Value value, Z3ScriptHandler z3Handler, Edge edge) {
		InterpolationContext ictx = z3Handler.getIctx();
		InvokeExpr iExpr = (InvokeExpr) value;

		Value firstBase = iExpr.getArg(this.argArray1);
		Value secondBase = iExpr.getArg(this.argArray2);

//		int firstMaxArraySize = z3Handler.getMaxArraySize().get(firstBase.toString()); 
//		int secondMaxArraySize = z3Handler.getMaxArraySize().get(secondBase.toString()); 
//		BoolExpr maxSizeEq = ictx.mkEq(ictx.mkInt(firstMaxArraySize), ictx.mkInt(secondMaxArraySize));

//		Expr firstExpr = z3Handler.convertValue(firstBase, false, edge, edge.getSource().getDistance());
//		Expr secondExpr = z3Handler.convertValue(secondBase, false, edge, edge.getSource().getDistance());

		ArrayExpr firstArray = this.getRealArray((Local)firstBase, edge, z3Handler); 
		ArrayExpr secondArray = this.getRealArray((Local)secondBase, edge, z3Handler); 
//		BoolExpr arrayEq = ictx.mkEq(firstArray, secondArray);
		
		
		Expr extIndex = ictx.mkArrayExt(firstArray, secondArray);
		BoolExpr firstEqSecond = ictx.mkEq(firstArray, secondArray);

//		BoolExpr wholeExpr = null;
//		for(int i=0; i < Math.min(firstMaxArraySize, secondMaxArraySize); i++) {
//			Expr firstSelect = ictx.mkSelect(firstArray, ictx.mkInt(i));
//			Expr secondSelect = ictx.mkSelect(secondArray, ictx.mkInt(i));
//			BoolExpr selectEq = ictx.mkEq(firstSelect, secondSelect);
//
//			if(wholeExpr == null)
//				wholeExpr = ictx.mkAnd(maxSizeEq, selectEq);
//			else
//				wholeExpr = ictx.mkAnd(wholeExpr, selectEq);
//		}

		IntExpr i = ictx.mkIntConst("i");
		Expr firstSelect = ictx.mkSelect(firstArray, i);
		LogUtils.detailln("firstSelect=" + firstSelect);
		Expr secondSelect = ictx.mkSelect(secondArray, i);
		LogUtils.detailln("secondSelect=" + secondSelect);
		BoolExpr eq = ictx.mkEq(firstSelect, secondSelect);
		BoolExpr notEq = ictx.mkNot(ictx.mkEq(firstSelect, secondSelect));
		LogUtils.detailln("notEq=" + notEq);

		IntExpr[] xs2 = new IntExpr[1];
		xs2[0] = i;

//		BoolExpr forall = ictx.mkForall(xs2, eq, 1, null, null, null, null);
		//BoolExpr exists = ictx.mkExists(xs2, notEq, 0, null, null, null, null);
		//Expr arrayExt = ictx.mkArrayExt(firstSelect, secondSelect);

//		Expr cond = ictx.mkITE(wholeExpr, ictx.mkInt(1), ictx.mkInt(0));
		Expr cond = ictx.mkITE(firstEqSecond, ictx.mkInt(1), ictx.mkInt(0));
		LogUtils.detailln(cond);

		/////////////////////////////


		return cond;
	}

	private ArrayExpr getRealArray(Local local, Edge edge, Z3ScriptHandler z3Handler) {
		Expr baseZ3 = z3Handler.convertValue(local, false, edge, edge.getSource().getDistance());
		Type type = local.getType();
		String typeName = type.toString();
		String realName = this.getArrayPrefix() + typeName;
		ArrayExpr realArray = (ArrayExpr) z3Handler.getGlobal().get(realName);
		ArrayExpr resultArray = (ArrayExpr) z3Handler.getIctx().mkSelect(realArray, baseZ3);
		return resultArray;
	}

	private String getArrayPrefix() {
		return "ArrayMem_";
	}
}
