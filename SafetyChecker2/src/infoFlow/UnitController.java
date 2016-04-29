package infoFlow;

import soot.Unit;
import soot.jimple.InvokeStmt;
import soot.jimple.Stmt;

public class UnitController {

	private static final String ERRORFUNCTION = "ErrorFunction";


	public static boolean isErrorUnit(Unit u) {
		if(u instanceof InvokeStmt){
			String sign = ((InvokeStmt)u).getInvokeExpr().getMethod().getSignature();
			if(sign.contains(ERRORFUNCTION))
				return true;	
		}
		return false;	
	}
}
