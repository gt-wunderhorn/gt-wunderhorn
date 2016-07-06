package safetyChecker.z3ScriptManager;

import java.util.HashSet;
import java.util.Map;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

import safetyChecker.Edge;
import safetyChecker.NewSort;
import safetyChecker.utilities.LogUtils;

import soot.Local;
import soot.SootMethod;
import soot.Type;
import soot.Value;
import soot.ValueBox;
import soot.jimple.InvokeExpr;

public class Z3JavaStringLibrary {

	private int arg0Index = 0;
	private int jimpleLocalBoxIndex = 1;

	private static final String STRING_CHARAT = "<java.lang.String: char charAt(int)>";

	private static final HashSet<String> STRING_LIBRARY_DB = new HashSet<String>();

	public Z3JavaStringLibrary() {
		STRING_LIBRARY_DB.add(STRING_CHARAT);
	}

	public boolean isJavaStringLibrary(Value value) {
		if(STRING_LIBRARY_DB.contains(this.getSootMethod(value).toString()))
			return true;
		return false;
	}	

	private SootMethod getSootMethod(Value value) {
		return ((InvokeExpr)value).getMethod();
	}

	public Expr createStringEquality(Value value, Z3ScriptHandler z3Handler, Edge edge) {
		if(this.getSootMethod(value).toString().equals(Z3JavaStringLibrary.STRING_CHARAT))
			return charAtEquality(value, z3Handler, edge);
		return null;
	}

	private Expr charAtEquality(Value value, Z3ScriptHandler z3Handler, Edge edge) {
		InterpolationContext ictx = z3Handler.getIctx();
		Map<String, Sort> newSortMap = z3Handler.getNewSortMap();


		InvokeExpr iExpr = (InvokeExpr) value;
		Value arg0 = iExpr.getArg(this.arg0Index);
		Expr arg0Exr = z3Handler.convertValue(arg0, false, edge, edge.getSource().getDistance());
			

		ValueBox valueBox = iExpr.getUseBoxes().get(this.jimpleLocalBoxIndex);
		Value callerObject = valueBox.getValue();
		
		String typeName = callerObject.getType().toString();
		String oldname = "realString";
		Sort newSort = newSortMap.get(typeName);

		ArrayExpr oldArray = (ArrayExpr) z3Handler.getGlobal().get(oldname);
		NewSort s2 = z3Handler.getSortId().get(typeName);
		Local local = (Local) callerObject ;
		String valueName = local.getName() + edge.getProgramTree().getProgramDefinition(); 
		Expr a = ictx.mkConst(valueName, newSort);
		Expr result = z3Handler.getIctx().mkSelect(oldArray, s2.getId(a));
		Expr result2 = z3Handler.getIctx().mkSelect((ArrayExpr)result, arg0Exr);

		return result2; 
	}
}
