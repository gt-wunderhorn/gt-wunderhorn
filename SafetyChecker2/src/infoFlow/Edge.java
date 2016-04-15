package infoFlow;

import soot.Unit;

public class Edge {

	//private String statement;
	private Unit unit;
	private Vertex v; // entering vertex
	private Vertex w; // exiting vertex

	public Unit getUnit() { return this.unit; }	
	public void setUnit(Unit unit) { this.unit = unit; }
	public Vertex getV() { return this.v; }
	public void setV(Vertex v) { this.v = v; }
	public Vertex getW() { return this.w; }
	public void setW(Vertex w) { this.w = w; }

}
