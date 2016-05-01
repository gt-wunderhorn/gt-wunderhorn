package infoFlow;

import java.util.Map;

import infoFlow.exception.ErrorLocationNotFoundException;
import infoFlow.exception.MainFunctionNotFoundException;

import soot.Body;
import soot.Unit;
import soot.jimple.AssignStmt;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;

public class UnitController {

	private static final String ERRORFUNCTION = "ErrorFunction";

	public static void analyzeVertex(Vertex v, Map<String, Body> stores)
			throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		for(Edge e : v.getIncomingEdges()) { 
			Unit u = e.getUnit();
			if(isErrorUnit(u)) { v.setErrorLocation(true); e.setErrorEdge(true); }
			if(isSubFunctionUnit(u, stores, e)) { v.setSubFunction(true); e.setSubFunction(true); }
		}
	}

	private static boolean isSubFunctionUnit(Unit u, Map<String, Body> stores, Edge e)
			throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		if(u instanceof InvokeStmt) { 
			String sign = getMethodSignature(u);
			if(stores.containsKey(sign)) {
				//LogUtils.detailln("SubFunction found : " + sign);
				//ExceptionalUnitGraph cfg = new ExceptionalUnitGraph(stores.get(sign));
				//HelpTree helpTree = new HelpTree(cfg);
				e.setProgramTree(new ProgramTree(stores, sign, false));
				return true;							
			}					
		}	
		return false;
	}

	public static boolean isErrorUnit(Unit u) {
		if(u instanceof InvokeStmt){
			String sign = getMethodSignature(u); //((InvokeStmt)u).getInvokeExpr().getMethod().getSignature();
			if(sign.contains(ERRORFUNCTION))
				return true;	
		}
		return false;	
	}

	private static String getMethodSignature(Unit u) {
		String sign = "";
		if(u instanceof InvokeStmt) sign = ((InvokeStmt)u).getInvokeExpr().getMethod().getSignature();
		else if(u instanceof AssignStmt) sign = ((InvokeExpr)((AssignStmt)u).getRightOp()).getMethod().getSignature();
		return sign;
	}
}
