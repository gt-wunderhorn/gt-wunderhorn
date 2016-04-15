package infoFlow;

public class Vertex {

	private boolean covered = false;
	private Edge edge;
	private Vertex nextVertex;
	private Vertex previousVertex;
	private String psi = "false";


        public boolean isCovered() { return this.covered; }
	public void setCovered(boolean covered) { this.covered = covered; }
	public Edge getEdge() { return this.edge; }
	public void setEdge(Edge edge) { this.edge = edge; }
	public Vertex getNextVertex() { return this.nextVertex; }
	public void setNextVertex(Vertex nextVertex) { this.nextVertex = nextVertex; } 
	
	public String getSigh() { return this.psi; }
	public void setSight(String psi) { this.psi = psi; }
	

}
