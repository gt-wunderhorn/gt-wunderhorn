package infoFlow;

import soot.Unit;

public class Edge {

	private Unit unit;
	private Vertex v; // entering vertex
	private Vertex w; // exiting vertex
	private ProgramTree programTree;

	private boolean returnEdge = false;
	private boolean errorEdge = false;
	private boolean subFunction = false;
	private boolean sinkEdge = false;
	private boolean sourceEdge = false;

	public Edge(Unit unit) { this.unit = unit; }

	public Unit getUnit() { return this.unit; }
	public void setUnit(Unit unit) { this.unit = unit; }

	public Vertex getV() { return this.v; }
	public void setV(Vertex v) { this.v = v; }
	
	public Vertex getW() { return this.w; }
	public void setW(Vertex w) { this.w = w; }
	
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

	public ProgramTree getProgramTree() { return this.programTree; }
	public void setProgramTree(ProgramTree programTree) { this.programTree = programTree; }

	public String toString() { return unit.toString(); }

}
