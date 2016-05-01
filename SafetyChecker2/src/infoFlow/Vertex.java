package infoFlow;

import java.util.HashSet;
import java.util.Set;

public class Vertex {

	private boolean covered = false;
	private boolean returnLocation = false;
	private boolean headLocation = false;
	private boolean errorLocation = false;
	private boolean subFunction = false;
	private Edge outgoingEdge;
	private Set<Edge> incomingEdges = new HashSet<Edge>();
	private Vertex nextVertex;
	private Set<Vertex> previousVertexSet = new HashSet<Vertex>();
	//private Set<Vertex> coveredPreviousVertexSet = new HashSet<Vertex>();
	//private Set<Vertex> uncoveredPreviousVertexSet = new HashSet<Vertex>();
	private boolean psi = false;
	private int distance = 0;


        public boolean isCovered() { return this.covered; }
	public void setCovered(boolean covered) { this.covered = covered; }

	public boolean isReturnLocation() { return this.returnLocation; }
	public void setReturnLocation(boolean returnLocation) { this.returnLocation = returnLocation; }
	
	public boolean isHeadLocation() { return this.headLocation; }
	public void setHeadLocation(boolean headLocation) { this.headLocation = headLocation; }
	
	public boolean isErrorLocation() { return this.errorLocation; }
	public void setErrorLocation(boolean errorLocation) { this.errorLocation = errorLocation; }
	
	public boolean isSubFunction() { return this.subFunction; }
	public void setSubFunction(boolean subFunction) { this.subFunction = subFunction; }
	
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

}
