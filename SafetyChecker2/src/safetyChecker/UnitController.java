package safetyChecker;

import java.util.HashMap;
import java.util.Map;

import safetyChecker.exception.ErrorLocationNotFoundException;
import safetyChecker.exception.MainFunctionNotFoundException;

import soot.Body;
import soot.Unit;
import soot.Value;
import soot.jimple.AssignStmt;
import soot.jimple.IfStmt;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;
import soot.jimple.NewExpr;
import soot.jimple.internal.JAssignStmt;

public class UnitController {


	public static final String ERRORLABEL = "ErrorLable";
	public static final String OBJECTINVOKE = "<java.lang.Object: void <init>";
	public static final String NONSENSCOMPARE = "java.lang.String: boolean equals";
	public static final String NOTINVOKESIGNATURE = "java.lang.Object: void <init>()>";

	private static final String SEND_TEXT_MESSAGE_SIGNATURE = "sendTextMessage";
       	private static final Integer SEND_TEXT_MESSAGE_PARAM_NO = 2;
	
	private static final String AND_UTIL_LOG_SIGNATURE = "android.util.Log";
	private static final Integer AND_UTIL_LOG_PARAM_NO = 1;

	private static final String SYSTEM_ARRAYCOPY_SIGNATURE = "<java.lang.System: void arraycopy(java.lang.Object,int,java.lang.Object,int,int)>";
	private static final String ARRAYS_EQUALS_SIGNATURE = "<java.util.Arrays: boolean equals(int[],int[])>";

	public static final String[] sinkSignatureDB = { SEND_TEXT_MESSAGE_SIGNATURE, AND_UTIL_LOG_SIGNATURE };
	public static final String[] sourceSignatureDB = {"getSimSerialNumber", "getDeviceId" }; 
	public static final Map<String, Integer> sensitiveParameterMap = new HashMap<String, Integer>();

	public UnitController () {
		sensitiveParameterMap.put(SEND_TEXT_MESSAGE_SIGNATURE, SEND_TEXT_MESSAGE_PARAM_NO);
		sensitiveParameterMap.put(AND_UTIL_LOG_SIGNATURE, AND_UTIL_LOG_PARAM_NO);
	}
	
	public void analyzeEdge(Edge e, Map<String, Body> stores) throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		Unit u = e.getUnit();
		e.setErrorEdge(this.isErrorUnit(u));
		e.setSubFunction(this.isSubFunctionUnit(u, stores, e));
		e.setSinkEdge(this.isSinkInvoke(u));
		e.setSourceEdge(this.isSourceInvoke(u));
		e.setObjectEdge(this.isObjectInvoke(u));
		e.setNewEdge(this.isNewInvoke(u));
		e.setArrayCopyEdge(this.isArrayCopyInvoke(u));
		e.setControlLocation(this.isControlLocation(u));

		if(e.isErrorEdge() || (e.getTarget().getOutgoingEdge() != null && e.getTarget().getOutgoingEdge().isInErrorPath())){
			e.setInErrorPath(true);
		}
	}

	private boolean isArrayCopyInvoke(Unit u) {
		if(u instanceof InvokeStmt && this.getMethodSignature(u).equals(this.SYSTEM_ARRAYCOPY_SIGNATURE)) 
			return true;
		return false;
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

	private boolean isSubFunctionUnit(Unit u, Map<String, Body> stores, Edge e) throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		if(isInvoke(u)) {
			String sign = getMethodSignature(u);
			if(!sign.contains(ERRORLABEL) && stores.containsKey(sign)) {
				LogUtils.warningln("SubFunction found : " + sign);
				e.setProgramTree(new ProgramTree(stores, sign, false));
				return true;							
			}					
		}	
		return false;
	}

	public boolean isErrorUnit(Unit u) {
		if(isInvoke(u)) {
			String sign = getMethodSignature(u); 
			if(sign.contains(ERRORLABEL))
				return true;	
		}
		return false;	
	}
	
	public boolean isObjectInvoke(Unit u) {
		if(isInvoke(u) && !(u instanceof JAssignStmt)) {
			InvokeStmt istmt = (InvokeStmt) u;
			String signature = istmt.getInvokeExpr().getMethod().getSignature();
			if(signature.contains(OBJECTINVOKE))
				return true;
		}
		return false;
	}

	public boolean isNewInvoke(Unit u) {
		if(u instanceof AssignStmt) {
			AssignStmt aStmt = (AssignStmt) u;
			Value right = aStmt.getRightOp();
			if(right instanceof NewExpr) {
				System.out.println("UnitController.isNewInvoke is not completed yet.");
				System.exit(0);				
			}
		}
		return false;
	}

	public boolean isNonSenseCompare(Unit u) {
		if(isInvoke(u)) {
			String sign = getMethodSignature(u);
			if(sign.contains(NONSENSCOMPARE))
				return true;		
		}	
		return false;
	}

	private boolean isCalleeFunction(Unit u) {
		if(isInvoke(u) && u instanceof AssignStmt) {
			String sign = this.getMethodSignature(u);
		}	
		return false;
	}

	public boolean isInvoke(Unit u) {
		String sign = this.getMethodSignature(u);
		if(u instanceof InvokeStmt && !sign.equals(this.NOTINVOKESIGNATURE)) {
			return true;
		} else if (u instanceof AssignStmt) {
			Value right = ((AssignStmt)u).getRightOp();
			if(right instanceof InvokeExpr && !sign.equals(this.NOTINVOKESIGNATURE))
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

	public static boolean isArraysEqualsInvoke(Value right) {
		if(right instanceof InvokeExpr) {
			String sign = ((InvokeExpr) right).getMethod().getSignature();
			if(sign.equals(ARRAYS_EQUALS_SIGNATURE))
				return true;
		}	
		return false;
	}	

	public static boolean isControlLocation(Unit unit) {
		if(unit instanceof IfStmt)
			return true;
		return false;	
	}

//	public void analyzeVertex(Vertex v, Map<String, Body> stores) throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
//		for(Edge e : v.getIncomingEdges()) { 
//			Unit u = e.getUnit();
//			if(isErrorUnit(u)) { v.setErrorLocation(true); e.setErrorEdge(true); }
//			if(isSubFunctionUnit(u, stores, e)) { v.setSubFunction(true); e.setSubFunction(true); }
//			if(isSinkInvoke(u)) { v.setSinkLocation(true); e.setSinkEdge(true); }
//			if(isSourceInvoke(u)) { v.setSourceLocation(true); e.setSourceEdge(true); } 
//		}
//	}
}
