package infoFlow;

import java.util.Map;

import infoFlow.exception.ErrorLocationNotFoundException;
import infoFlow.exception.MainFunctionNotFoundException;

import soot.Body;
import soot.Unit;
import soot.Value;
import soot.jimple.AssignStmt;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;

public class UnitController {

	private static final String ERRORFUNCTION = "ErrorFunction";
	private static final String[] sinkSignatureDB = { "sendTextMessage", "android.util.Log" };
	private static final String[] sourceSignatureDB = {"getSimSerialNumber", "getDeviceId" }; 

	public static void analyzeEdge(Edge e, Map<String, Body> stores) throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		Unit u = e.getUnit();
		e.setErrorEdge(isErrorUnit(u));
		e.setSubFunction(isSubFunctionUnit(u, stores, e));
		e.setSinkEdge(isSinkInvoke(u));
		e.setSourceEdge(isSourceInvoke(u));
	}

	public static void analyzeVertex(Vertex v, Map<String, Body> stores) throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		for(Edge e : v.getIncomingEdges()) { 
			Unit u = e.getUnit();
			if(isErrorUnit(u)) { v.setErrorLocation(true); e.setErrorEdge(true); }
			if(isSubFunctionUnit(u, stores, e)) { v.setSubFunction(true); e.setSubFunction(true); }
			if(isSinkInvoke(u)) { v.setSinkLocation(true); e.setSinkEdge(true); }
			if(isSourceInvoke(u)) { v.setSourceLocation(true); e.setSourceEdge(true); } 
		}
	}

	private static boolean isSourceInvoke(Unit u) {
		if(u instanceof AssignStmt) {
			String sign = getMethodSignature(u);
			for(String sourceSignature : sourceSignatureDB) 
				if(sign.contains(sourceSignature)) return true; 
		}
		return false;	
	}

	private static boolean isSinkInvoke(Unit u) {
		LogUtils.debugln("isSinkInvoke:" + u);
		String sign = getMethodSignature(u);
		for(String sinkSignature : sinkSignatureDB)
			if(sign.contains(sinkSignature)) return true;	
		return false;
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
			String sign = getMethodSignature(u); 
			if(sign.contains(ERRORFUNCTION))
				return true;	
		}
		return false;	
	}

	private static String getMethodSignature(Unit u) {
		String sign = "";
		if(u instanceof InvokeStmt) 
			sign = ((InvokeStmt)u).getInvokeExpr().getMethod().getSignature();
		else if(u instanceof AssignStmt) { 
			Value right = ((AssignStmt)u).getRightOp();
			if(right instanceof InvokeExpr)
				sign = ((InvokeExpr)right).getMethod().getSignature();
		}
		return sign;
	}
}
