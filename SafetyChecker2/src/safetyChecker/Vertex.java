package safetyChecker;

import java.util.HashSet;
import java.util.Set;

import com.microsoft.z3.BoolExpr;

public class Vertex {

	private boolean returnLocation = false;
	private boolean entryLocation = false;
	private boolean errorLocation = false;
	private boolean subFunction = false;
	private boolean sinkLocation = false;
	private boolean sourceLocation = false;
	private boolean isInErrorPath = false; 
	private boolean vertexDone = false;

	private Edge outgoingEdge;
	private Set<Edge> incomingEdges = new HashSet<Edge>();
	private Vertex nextVertex;
	private Set<Vertex> previousVertexSet = new HashSet<Vertex>();

	private boolean psi = false;
	private int distance = 0;
	private int locationNumber = 0;
	private int rootNumber = 0;
	private BoolExpr invariant;

	public boolean isReturnLocation() { return this.returnLocation; }
	public void setReturnLocation(boolean returnLocation) { this.returnLocation = returnLocation; }
	
	public boolean isEntryLocation() { return this.entryLocation; }
	public void setEntryLocation(boolean entryLocation) { this.entryLocation = entryLocation; }
	
	public boolean isErrorLocation() { return this.errorLocation; }
	public void setErrorLocation(boolean errorLocation) { this.errorLocation = errorLocation; }
	
	public boolean isSubFunction() { return this.subFunction; }
	public void setSubFunction(boolean subFunction) { this.subFunction = subFunction; }
	
	public boolean isSinkLocation() { return this.sinkLocation; }
	public void setSinkLocation(boolean sinkLocation) { this.sinkLocation = sinkLocation; }

	public boolean isSourceLocation() { return this.sourceLocation; }
	public void setSourceLocation(boolean sourceLocation) { this.sourceLocation = sourceLocation; }

	public boolean isInErrorPath() { return this.isInErrorPath; }
	public void setInErrorPath(boolean isInErrorPath) { this.isInErrorPath = isInErrorPath; }

	public boolean isVertexDone() { return this.vertexDone; }
	public void setVertexDone(boolean vertexDone) { this.vertexDone = vertexDone; }	

	public Edge getOutgoingEdge() { return this.outgoingEdge; }
	public void setOutgoingEdge(Edge outgoingEdge) { this.outgoingEdge = outgoingEdge;}
	
	public Set<Edge> getIncomingEdges() { return this.incomingEdges; }
	public void addIncomingEdge(Edge incomingEdge) { this.incomingEdges.add(incomingEdge); }
	
	public Vertex getNextVertex() { return this.nextVertex; }
	public void setNextVertex(Vertex nextVertex) { this.nextVertex = nextVertex; } 
	
	public Set<Vertex> getPreviousVertexSet() { return this.previousVertexSet; }
	public void addPreviousVertex(Vertex v) { this.previousVertexSet.add(v); }
	
	public boolean isSigh() { return this.psi; }
	public void setSight(boolean psi) { this.psi = psi; }

	public int getDistance() { return this.distance; }
	public void setDistance(int distance) { this.distance = distance; } 	

	public int getLocationNumber() { return this.locationNumber; }
	public void setLocationNumber(int locationNumber) { this.locationNumber = locationNumber; } 	

	public BoolExpr getInvariant() { return this.invariant; }
	public void setInvariant(BoolExpr invariant) { this.invariant = invariant; }

	public String toString() { return "L" + locationNumber + "-D" + distance; }
}
