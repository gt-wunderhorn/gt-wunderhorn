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
	private CoverRelation coverRelation;

	Expr[] from = null;
	Expr[] to = null;

	public InterpolationHandler(InterpolationContext ictx, Z3ScriptHandler z3Handler, CoverRelation coverRelation) { 
		this.ictx = ictx;
		this.z3Handler = z3Handler;
		this.coverRelation = coverRelation;
	}

	public boolean createInterpolant(Vertex errorRoot) { 
		LogUtils.debugln(">>>>>> InterpolationHandler.createInterpolant");

//	BoolExpr pathFormula=this.ictx.MkInterpolant(errorRoot.getOutgoingEdge().getZ3Expr());
		BoolExpr pathFormula = errorRoot.getOutgoingEdge().getZ3Expr();

		LogUtils.debugln("******root=" + errorRoot);
		Vertex currentVertex = errorRoot.getNextVertex();
		int rootDistance = errorRoot.getDistance();
		int halfDistance = rootDistance / 2;
		boolean isFeasible = true;
		ComputeInterpolantResult interpolantResult = null;
		Params params = this.ictx.mkParams();

		while(currentVertex != null) {
			Edge edge = currentVertex.getOutgoingEdge();
			if(edge.isErrorEdge()) 
				break;

			BoolExpr z3Epxr = edge.getZ3Expr();
			LogUtils.debugln("z3Expr = " + z3Epxr);

			BoolExpr conjunction = this.ictx.mkAnd(z3Epxr, pathFormula);
			if(edge.isControlLocation()) { 
				pathFormula = this.ictx.MkInterpolant(conjunction);
				if(currentVertex.getDistance() <= halfDistance) {
					interpolantResult = this.ictx.ComputeInterpolant(pathFormula, params);
					Z3_lbool status = interpolantResult.status;
					if(status == Z3_lbool.Z3_L_TRUE) {
						halfDistance = halfDistance / 2;
					} else if (status == Z3_lbool.Z3_L_FALSE) {
						isFeasible = false;
						break;
					}
				}
			} else
				pathFormula = conjunction;

			currentVertex = currentVertex.getNextVertex();
		}

		LogUtils.debugln("------------------pathformula----------------");
		LogUtils.debugln(pathFormula);
		LogUtils.debugln("------------------pathformula----------------");
		if(isFeasible)
			interpolantResult = this.ictx.ComputeInterpolant(pathFormula, params);
		BoolExpr[] invariantList = interpolantResult.interp;
		this.generateNameMapping();
		this.updateInvariant(errorRoot, invariantList, isFeasible);

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

	private void updateInvariant(Vertex errorRootVertex, BoolExpr[] invariantList, boolean isFeasible) {
		if(invariantList != null) {
			LogUtils.debugln("invariantList size is " + invariantList.length);
			Vertex vertex = errorRootVertex;//.getNextVertex();
			int index = 0;
			BoolExpr falseExpr = this.ictx.mkFalse();
			while(vertex.getOutgoingEdge() != null) {
				if(vertex.getOutgoingEdge().isControlLocation()) {
					Vertex nextLocation = vertex.getNextVertex();
					BoolExpr currentInvariant = nextLocation.getInvariant();
					BoolExpr z3Invariant = null;
					if(index >= invariantList.length && !isFeasible)
						z3Invariant = falseExpr;
					else if (index >= invariantList.length && isFeasible)
						LogUtils.fatalln("Check the invariant list");
					else 
						z3Invariant = invariantList[index];
					BoolExpr newInvariant = (BoolExpr) z3Invariant.substitute(this.from, this.to);
					
					if(nextLocation.getInvariant() == null)
						nextLocation.setInvariant(newInvariant);
					else {
						BoolExpr disjunction = this.ictx.mkOr(newInvariant, currentInvariant);
						BoolExpr simplified = (BoolExpr) disjunction.simplify();
						nextLocation.setInvariant(simplified);
						// vertex invariant is weakedned clear the vertexes covered by weakened invariants
						coverRelation.clearCoverRelation(nextLocation);
					}
					index++; 
				}

				vertex = vertex.getNextVertex();
			}
		} else { 
			LogUtils.warningln("invariantList is null");
			Vertex vertex = errorRootVertex;
			while(vertex.getOutgoingEdge() != null) {
				if(vertex.getOutgoingEdge().isControlLocation()) {
					BoolExpr trueExpr = this.ictx.mkTrue();
					Vertex nextLocation = vertex.getNextVertex();
					BoolExpr currentInvariant = nextLocation.getInvariant();
					if(currentInvariant == null)
						nextLocation.setInvariant(trueExpr);
					else {
						BoolExpr disjunction = this.ictx.mkOr(trueExpr, currentInvariant);
			//			BoolExpr simplified = (BoolExpr) disjunction.simplify();
					       nextLocation.setInvariant(disjunction);	
					}
				}
				vertex = vertex.getNextVertex();
			}
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
