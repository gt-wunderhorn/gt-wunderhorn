package infoFlow;

import java.util.HashSet;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.InterpolationContext.ComputeInterpolantResult;
import com.microsoft.z3.Params;

import soot.jimple.IfStmt;

public class InterpolationHandler {

	private InterpolationContext ictx;

	public InterpolationHandler(InterpolationContext ictx) { 
		this.ictx = ictx;
	}

	public BoolExpr createInterpolant(HashSet<Vertex> errorRootSet) { 
		LogUtils.infoln(">>>>>> InterpolationHandler.createInterpolant");
		BoolExpr interpolant = null; 
		int rootCounter = 1;
		BoolExpr disjunction = null; 
		for(Vertex vertex : errorRootSet) {
			LogUtils.infoln("root # " + rootCounter++);
			while(vertex != null) {
				Edge edge = vertex.getOutgoingEdge();
				if(edge.isErrorEdge()) 
					break;

				LogUtils.infoln("----------------");
				BoolExpr currentInterpolant = vertex.getInvariant();
				LogUtils.infoln("currentInterpolant = " + currentInterpolant); 
				BoolExpr z3Epxr = edge.getZ3Expr();
				LogUtils.infoln("z3Expr = " + z3Epxr);
				if(vertex.isHeadLocation())
					disjunction = z3Epxr;
				else
					disjunction = this.ictx.mkOr(z3Epxr, currentInterpolant);

				LogUtils.infoln("disjunction = " + disjunction);

				interpolant = this.ictx.MkInterpolant(disjunction);
				LogUtils.infoln("interpolant = " + interpolant);
				edge.getTarget().setInvariant(interpolant);
				edge.getTarget().setInvariant(disjunction);

				vertex = vertex.getNextVertex();
			}
//				interpolant = this.ictx.MkInterpolant(disjunction);
//				vertex.getNextVertex().setInvariant(interpolant);
		}			

		LogUtils.infoln("********** interpolant*************");
		LogUtils.infoln(interpolant);
		Params params = this.ictx.mkParams();
		ComputeInterpolantResult interpolantResult = this.ictx.ComputeInterpolant(interpolant, params);
		LogUtils.infoln("result=" + interpolantResult.status);
		LogUtils.infoln("<<<<<< InterpolationHandler.createInterpolant");
		return null;
	}	

	private void addInvariant(Vertex vertex, BoolExpr interpolant) {
		
	}

	public BoolExpr getTrueInvariant() { return this.ictx.mkTrue(); }
	public BoolExpr getFalseInvariant() { return this.ictx.mkFalse(); }


	

}
