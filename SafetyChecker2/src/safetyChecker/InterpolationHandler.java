package safetyChecker;

import java.util.Map;
import java.util.Map.Entry;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.InterpolationContext.ComputeInterpolantResult;
import com.microsoft.z3.Params;
import com.microsoft.z3.Sort;
import com.microsoft.z3.enumerations.Z3_lbool;

public class InterpolationHandler {

	private InterpolationContext ictx;
	private Z3ScriptHandler z3Handler;

	Expr[] from = null;
	Expr[] to = null;

	public InterpolationHandler(InterpolationContext ictx, Z3ScriptHandler z3Handler) { 
		this.ictx = ictx;
		this.z3Handler = z3Handler;
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
		this.generateNameMapping();
		this.updateInvariant(errorRoot, invariantList);

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
				BoolExpr invariant = invariantList[index];
				BoolExpr newInvariant = (BoolExpr) invariant.substitute(this.from, this.to);

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

	private void generateNameMapping() {
		Map<String, String> substitute = this.z3Handler.getSubstitute();
		Map<String, Sort> substituteSort = this.z3Handler.getSubstituteSort();
		this.from = new Expr[substitute.size()];
		this.to = new Expr[substitute.size()];

		int i = 0;
		for(Entry<String, String> entry : substitute.entrySet()) {
			String fromStr = entry.getKey();
			String toStr = entry.getValue();

			Sort sort = substituteSort.get(fromStr);
			if(sort == null) {
				LogUtils.debugln("sort is null for " + fromStr);
				sort = this.ictx.mkIntSort();
			}

			from[i] = this.ictx.mkConst(fromStr, sort);
			to[i] = this.ictx.mkConst(toStr, sort);
			i++;
		}	
	}

	public BoolExpr getTrueInvariant() { return this.ictx.mkTrue(); }
	public BoolExpr getFalseInvariant() { return this.ictx.mkFalse(); }
}
