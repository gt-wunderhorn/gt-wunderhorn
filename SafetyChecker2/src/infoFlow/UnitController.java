package infoFlow;

import java.util.HashMap;
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

	private InterpolationHandler itpHandler = new InterpolationHandler(); 

	public static final String ERRORFUNCTION = "ErrorFunction";

	private static final String SEND_TEXT_MESSAGE_SIGNATURE = "sendTextMessage";
       	private static final Integer SEND_TEXT_MESSAGE_PARAM_NO = 2;
	
	private static final String AND_UTIL_LOG_SIGNATURE = "android.util.Log";
	private static final Integer AND_UTIL_LOG_PARAM_NO = 1;

	public static final String[] sinkSignatureDB = { SEND_TEXT_MESSAGE_SIGNATURE, AND_UTIL_LOG_SIGNATURE };
	public static final String[] sourceSignatureDB = {"getSimSerialNumber", "getDeviceId" }; 
	public static final Map<String, Integer> sensitiveParameterMap = new HashMap<String, Integer>();

	public UnitController () {
		sensitiveParameterMap.put(SEND_TEXT_MESSAGE_SIGNATURE, SEND_TEXT_MESSAGE_PARAM_NO);
		sensitiveParameterMap.put(AND_UTIL_LOG_SIGNATURE, AND_UTIL_LOG_PARAM_NO);
	}
	
	public void analyzeEdge(Edge e, Map<String, Body> stores) throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		Unit u = e.getUnit();
		e.setErrorEdge(isErrorUnit(u));
		e.setSubFunction(isSubFunctionUnit(u, stores, e));
		e.setSinkEdge(isSinkInvoke(u));
		e.setSourceEdge(isSourceInvoke(u));

		boolean converted = itpHandler.createZ3Script(e); 
		if(!converted)
			LogUtils.warning("WARNING: " + u + " cannot be converted to Z3 script");
	}

	public void analyzeVertex(Vertex v, Map<String, Body> stores) throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		for(Edge e : v.getIncomingEdges()) { 
			Unit u = e.getUnit();
			if(isErrorUnit(u)) { v.setErrorLocation(true); e.setErrorEdge(true); }
			if(isSubFunctionUnit(u, stores, e)) { v.setSubFunction(true); e.setSubFunction(true); }
			if(isSinkInvoke(u)) { v.setSinkLocation(true); e.setSinkEdge(true); }
			if(isSourceInvoke(u)) { v.setSourceLocation(true); e.setSourceEdge(true); } 
		}
	}

	private boolean isSourceInvoke(Unit u) {
		if(u instanceof AssignStmt) {
			String sign = getMethodSignature(u);
			for(String sourceSignature : sourceSignatureDB) 
				if(sign.contains(sourceSignature)) return true; 
		}
		return false;	
	}

	private boolean isSinkInvoke(Unit u) {
		LogUtils.debugln("isSinkInvoke:" + u);
		String sign = getMethodSignature(u);
		for(String sinkSignature : sinkSignatureDB)
			if(sign.contains(sinkSignature)) return true;	
		return false;
	}

	private boolean isSubFunctionUnit(Unit u, Map<String, Body> stores, Edge e)
			throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		if(u instanceof InvokeStmt) { 
			String sign = getMethodSignature(u);
			if(stores.containsKey(sign)) {
				//LogUtils.detailln("SubFunction found : " + sign);
				//ExceptionalUnitGraph cfg = new ExceptionalUnitGraph(stores.get(sign));
				//HelpTree helpTree = new HelpTree(cfg);
				new ProgramTree(stores, sign, false);
				return true;							
			}					
		}	
		return false;
	}

	public boolean isErrorUnit(Unit u) {
		if(u instanceof InvokeStmt){
			String sign = getMethodSignature(u); 
			if(sign.contains(ERRORFUNCTION))
				return true;	
		}
		return false;	
	}

	public static String getMethodSignature(Unit u) {
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
