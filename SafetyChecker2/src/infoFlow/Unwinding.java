package infoFlow;

import java.util.Map;

import soot.Unit;
import soot.toolkits.graph.ExceptionalUnitGraph;

public class Unwinding {
	Map<Vertex, ProgramLocation> vertexMap;
	Map<Edge, Action> actionMap;
		

	public void expand(Vertex w, ExceptionalUnitGraph eug, Program p)  {
		if(!w.isCovered()) {	
			for(Unit action : eug.getPredsOf(w.getEdge().getUnit())) {
				Vertex v = new Vertex();
				Edge e = new Edge();

				e.setUnit(action);
				e.setV(v);
				e.setW(w);
				p.getEdgeSet().add(e);
				
				v.setEdge(e);
				v.setNextVertex(w);
				p.getVertexSet().add(v);

			}
		}
			
	}


}
