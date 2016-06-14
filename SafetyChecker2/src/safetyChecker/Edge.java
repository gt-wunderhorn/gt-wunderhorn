package safetyChecker;

import java.util.LinkedList;
import java.util.List;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;

import soot.Unit;

public class Edge {

	private Unit unit;
	private Vertex source; 
	private Vertex target;
	private ProgramTree programTree;
	private BoolExpr z3Expr;

	private boolean returnEdge = false;
	private boolean errorEdge = false;
	private boolean subFunction = false;
	private boolean sinkEdge = false;
	private boolean sourceEdge = false;
	private boolean objectEdge = false;
	private boolean newEdge = false;
	private boolean inErrorPath = false;
	private boolean arrayCopy = false;
///	private boolean calleeFunction = false;
	private List<Expr> parameterList = new LinkedList<Expr>();

	public Edge(Unit unit) { this.unit = unit; }

	public Unit getUnit() { return this.unit; }
	public void setUnit(Unit unit) { this.unit = unit; }

	public Vertex getSource() { return this.source; } 
	public void setSource(Vertex source) { this.source = source; }
	
	public Vertex getTarget() { return this.target; }
	public void setTarget(Vertex target) { this.target = target; }
	
	public boolean isReturnEdge() { return this.returnEdge; }
	public void setReturnEdge(boolean returnEdge) { this.returnEdge = returnEdge; }
	
	public boolean isErrorEdge() { return this.errorEdge; }
	public void setErrorEdge(boolean errorEdge) { this.errorEdge = errorEdge; }
	
	public boolean isSubFunction() { return this.subFunction; }
	public void setSubFunction(boolean subFunction) { this.subFunction = subFunction; }	
	
	public boolean isSinkEdge() { return this.sinkEdge; }
	public void setSinkEdge(boolean sinkEdge) { this.sinkEdge = sinkEdge; }
	
	public boolean isSourceEdge() { return this.sourceEdge; }
	public void setSourceEdge(boolean sourceEdge) { this.sourceEdge = sourceEdge; }

	public boolean isNewEdge() { return this.newEdge; }
	public void setNewEdge(boolean newEdge) { this.newEdge = newEdge; }

	public boolean isObjectEdge() { return this.objectEdge; }
	public void setObjectEdge(boolean objectEdge) { this.objectEdge = objectEdge; }

	public boolean isArrayCopyEdge() { return this.arrayCopy; }
	public void setArrayCopyEdge(boolean arrayCopy) { this.arrayCopy = arrayCopy; }

	public ProgramTree getProgramTree() { return this.programTree; }
	public void setProgramTree(ProgramTree programTree) { this.programTree = programTree; }

	public boolean isInErrorPath() { return this.inErrorPath; }
	public void setInErrorPath(boolean inErrorPath) { this.inErrorPath = inErrorPath; }

	public BoolExpr getZ3Expr() { return this.z3Expr; }
	public void setZ3Expr(BoolExpr z3Expr) { this.z3Expr = z3Expr; } 

	public List<Expr> getParameterList() { return this.parameterList; }
	public void addParameter(Expr parameter) { this.parameterList.add(parameter); }
	public void cleanParameterList() { this.parameterList = new LinkedList<Expr>(); }

//	public boolean isCalleeFunction() { return this.calleeFunction; }
//	public void setCalleeFunction(boolean calleeFunction) { this.calleeFunction = calleeFunction; }

	public String toString() { return unit.toString(); }

}