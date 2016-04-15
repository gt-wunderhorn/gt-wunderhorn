package infoFlow;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import soot.Body;

public class Program {

	private Set<ProgramLocation> locations;
	private Set<Action> actions;
	private ProgramLocation li;
	private ProgramLocation lf;

	private Map<String, Body> stores; 
	// for algorithm
	private Set<Vertex> vertexSet = new HashSet<Vertex>();
	private Set<Edge> edgeSet = new HashSet<Edge>(); 
	



	public Set<Vertex> getVertexSet() { return this.vertexSet; }
	public Set<Edge> getEdgeSet() { return this.edgeSet; }


	
}
