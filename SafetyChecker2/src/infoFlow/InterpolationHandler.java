package infoFlow;

import java.util.HashSet;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.InterpolationContext.ComputeInterpolantResult;
import com.microsoft.z3.Params;
import com.microsoft.z3.enumerations.Z3_lbool;

import soot.jimple.IfStmt;

public class InterpolationHandler {

	private InterpolationContext ictx;
	private boolean startRefine = false;

	public InterpolationHandler(InterpolationContext ictx) { 
		this.ictx = ictx;
	}

	public boolean createInterpolant(Vertex errorRoot) { 
		LogUtils.debugln(">>>>>> InterpolationHandler.createInterpolant");

		BoolExpr pathFormula = this.ictx.MkInterpolant(errorRoot.getOutgoingEdge().getZ3Expr());
				// this.ictx.mkAnd(errorRoot.getOutgoingEdge().getZ3Expr(), 
				//	this.ictx.mkTrue())); 

		LogUtils.infoln("******root=" + errorRoot);
		Vertex currentVertex = errorRoot.getNextVertex();
		while(currentVertex != null) {
			Edge edge = currentVertex.getOutgoingEdge();
			if(edge.isErrorEdge()) 
				break;

			BoolExpr z3Epxr = edge.getZ3Expr();
			LogUtils.debugln("z3Expr = " + z3Epxr);
//			if(currentVertex.isHeadLocation())
//				conjunction = this.ictx.mkAnd(z3Epxr, currentVertex.getInvariant());
//			else
			BoolExpr conjunction = this.ictx.mkAnd(z3Epxr, pathFormula);

			pathFormula = this.ictx.MkInterpolant(conjunction);
			currentVertex = currentVertex.getNextVertex();
		}

		Params params = this.ictx.mkParams();
		LogUtils.debugln("------------------pathformula----------------");
		LogUtils.debugln(pathFormula);
		LogUtils.debugln("------------------pathformula----------------");
		ComputeInterpolantResult interpolantResult = this.ictx.ComputeInterpolant(pathFormula, params);
		BoolExpr[] invariantList = interpolantResult.interp;
		updateInvariant(errorRoot, invariantList);

		Z3_lbool status = interpolantResult.status;
		boolean result = false;
		if(status == Z3_lbool.Z3_L_FALSE)
			result = false;
		else if(status == Z3_lbool.Z3_L_TRUE)
			result = true;

		LogUtils.debugln("interpolation result=" + result);
		LogUtils.debugln("<<<<<< InterpolationHandler.createInterpolant");
		return result;
	}

	private void updateInvariant(Vertex errorRootVertex, BoolExpr[] invariantList) {
		if(invariantList != null) {
			LogUtils.debugln("invariantList size is " + invariantList.length);
			int index = 0;
			Vertex vertex = errorRootVertex.getNextVertex();
			while(vertex != null && index < invariantList.length) { 
				BoolExpr currentInvariant = vertex.getInvariant();
				BoolExpr newInvariant = invariantList[index];
//				LogUtils.info(vertex.getOutgoingEdge());
//				LogUtils.fatal("****");
//				LogUtils.info(vertex.getOutgoingEdge().getZ3Expr());
//			        LogUtils.fatal("****");
//				if(newInvariant.toString().equals("false")) 
//			        	LogUtils.warningln(newInvariant);
//				else
//					LogUtils.infoln(newInvariant);


				if(vertex.getInvariant() == null) {
					vertex.setInvariant(newInvariant);
				} else {
					BoolExpr disjunction = this.ictx.mkOr(newInvariant, currentInvariant);
//					BoolExpr currentInterpolant = this.ictx.MkInterpolant(disjunction); 
					vertex.setInvariant(disjunction);	
				}
				index++;
				vertex = vertex.getNextVertex();
			}
		} else {
			LogUtils.warningln("invariantList is null");
		}
	}	

//	private void addInvariant(Vertex vertex, BoolExpr interpolant) {
//		BoolExpr currentInvariant = vertex.getInvariant();
//		if(true || currentInvariant == null || startRefine) {
//			vertex.setInvariant(interpolant);
//		} else {
//			BoolExpr disjunction = this.ictx.mkOr(interpolant, currentInvariant);
//			LogUtils.debugln("^^^^^ Disjunction ^^^^^ \n" + disjunction);
//			BoolExpr currentInterpolant = this.ictx.MkInterpolant(disjunction); 
//			vertex.setInvariant(currentInterpolant);	
//			startRefine = true;
//		}
//
//	}

	public BoolExpr getTrueInvariant() { return this.ictx.mkTrue(); }
	public BoolExpr getFalseInvariant() { return this.ictx.mkFalse(); }


	

}
