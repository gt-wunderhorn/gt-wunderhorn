package z3_helper;

import java.security.Signature;

import soot.Unit;
import soot.jimple.InvokeStmt;

public class ErrorHelper {
	static public boolean ifError(Unit u){
		if(u instanceof InvokeStmt){
			InvokeStmt iStmt=(InvokeStmt) u;
			String signature=iStmt.getInvokeExpr().getMethod().getSignature();
			if(signature.contains("toy_benchmark.ErrorFunction:")){
				return true;
			}
		}
		return false;
	}
	static public boolean ifObject(Unit u){
		if(u instanceof InvokeStmt){
			InvokeStmt iStmt=(InvokeStmt) u;
			String signatrue=iStmt.getInvokeExpr().getMethod().getSignature();
			if(signatrue.contains("<java.lang.Object: void <init>")){
				return true;
			}
		}
		return false;
	}
}
