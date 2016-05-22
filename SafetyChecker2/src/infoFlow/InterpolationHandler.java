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

	public boolean createInterpolant(HashSet<Vertex> errorRootSet) { 
		LogUtils.infoln(">>>>>> InterpolationHandler.createInterpolant");
		BoolExpr interpolant = null; 
		int rootCounter = 1;
		BoolExpr conjunction = null; 
		for(Vertex vertex : errorRootSet) {
			LogUtils.infoln("******root=" + vertex);
			if(vertex.getNextVertex().getInvariant() != null) {
				LogUtils.infoln("********passed root = " + vertex);
				continue;
			}
			while(vertex != null) {
				Edge edge = vertex.getOutgoingEdge();
				if(edge.isErrorEdge()) 
					break;

				BoolExpr currentInterpolant = vertex.getInvariant();
				BoolExpr z3Epxr = edge.getZ3Expr();
				LogUtils.debugln("z3Expr = " + z3Epxr);
				if(vertex.isHeadLocation())
					conjunction = z3Epxr;
				else
					conjunction = this.ictx.mkAnd(z3Epxr, currentInterpolant);

//				interpolant = this.ictx.MkInterpolant(conjunction);
//				LogUtils.debugln("interpolant = " + interpolant);

				addInvariant(edge.getTarget(), conjunction);
				vertex = vertex.getNextVertex();
			}
			startRefine = false;
			interpolant = this.ictx.MkInterpolant(conjunction);
			vertex.getNextVertex().setInvariant(interpolant);
		}			

//		LogUtils.infoln("********** interpolant*************");
//		LogUtils.infoln(interpolant);
		Params params = this.ictx.mkParams();
		ComputeInterpolantResult interpolantResult = this.ictx.ComputeInterpolant(interpolant, params);
		Z3_lbool status = interpolantResult.status;
		boolean result = false;
		if(status == Z3_lbool.Z3_L_FALSE)
			return false;
		else if(status == Z3_lbool.Z3_L_TRUE)
			return true;

		LogUtils.infoln("interpolation result=" + result);
		LogUtils.infoln("<<<<<< InterpolationHandler.createInterpolant");
		return result;
	}	

	private void addInvariant(Vertex vertex, BoolExpr interpolant) {
		BoolExpr currentInvariant = vertex.getInvariant();
		if(currentInvariant == null || startRefine) {
			vertex.setInvariant(interpolant);
		} else {
			BoolExpr disjunction = this.ictx.mkOr(interpolant, currentInvariant);
			LogUtils.debugln("^^^^^ Disjunction ^^^^^ \n" + disjunction);
			BoolExpr currentInterpolant = this.ictx.MkInterpolant(disjunction); 
			vertex.setInvariant(currentInterpolant);	
			startRefine = true;
		}

	}

	public BoolExpr getTrueInvariant() { return this.ictx.mkTrue(); }
	public BoolExpr getFalseInvariant() { return this.ictx.mkFalse(); }


	

}
