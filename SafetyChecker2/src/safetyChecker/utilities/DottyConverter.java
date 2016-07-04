package safetyChecker.utilities;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;

import safetyChecker.CoverRelation;
import safetyChecker.Edge;
import safetyChecker.Vertex;

import soot.Unit;
import soot.toolkits.graph.ExceptionalUnitGraph;

public class DottyConverter {

	private static BufferedWriter getBufferedWriter(String fileName) throws IOException {
			File file = new File(fileName);
			BufferedWriter writer = new BufferedWriter(new FileWriter(file));
			return writer;
	}

	public static void printErrorPaths(Queue<Vertex> queue, String fileName, CoverRelation coverRelation) {
		try{
			Map<Vertex, Vertex> coveredByMap = coverRelation.getCoveredByMap();

			BufferedWriter writer = getBufferedWriter(fileName);	
			writer.write("digraph { \n");
			writer.write("\tratio=\"fill\";\n\tsize=\"8.3,10.7!\";\n\tmargin=0;\n");

			while(!queue.isEmpty()) {
				Vertex v = queue.remove();
				Set<Vertex> vSet = v.getPreviousVertexSet();
				Edge e = v.getOutgoingEdge();

				String color = "";
				if(coverRelation.isAncestorCovered(v))
					color = "color =green, ";

				if(v.getDistance()!=0 )
					if(v.getOutgoingEdge().isInErrorPath()) { 
						Vertex w = v.getNextVertex();
						String source = v.toString();
						String target = w.toString();

						String inv4Edge = "";
						if(v.getInvariant() != null) {
							String inv = "**";
							if(!v.getInvariant().toString().contains("\n"))
								inv = v.getInvariant().toString();
							else
								inv4Edge = "\n** " + v.getInvariant().toString(); 

						       	source += "--" + inv;
							writer.write("\t\"" + source + "\"[shape=polygon, sides=5, peripheries=2, color=turquoise, style=filled]");
						} 
						if(w.getInvariant() != null) {
							String inv = "**";
							if(!w.getInvariant().toString().contains("\n"))
								inv = w.getInvariant().toString();
							target += "--" + inv;
						}
						if(v.getPreviousVertexSet().size()==0) {
							writer.write("\t\"" + source + "\"[shape=rectangle, color=sandybrown, style=filled]");
						}

						writer.write("\t\"" + source + "\" -> \"" + target + "\"[" + color + "label=\"" + e + "--" /*+ e.getZ3Expr()*/ + inv4Edge  + "\"];\n");

//						writer.write("\t\"" + w.getNextVertex() + "\" -> \"" + w + "\"[" + color + "label=\"" + e + "--" + e.getZ3Expr() + "\n**" + w.getInvariant() + "\"];\n");
						
					}
				for(Vertex v2 : vSet) { 
					queue.add(v2);
				}
			}
			
			for(Entry<Vertex, Vertex> entry : coveredByMap.entrySet()) {
				Vertex coveredVertex = entry.getKey();
				Vertex coveringVertex = entry.getValue();

				
				String source = coveredVertex.toString();
				String target = coveringVertex.toString();

				if(coveredVertex.getInvariant() != null) { 
					String inv = "**";
					if(!coveredVertex.getInvariant().toString().contains("\n"))
						inv = coveredVertex.getInvariant().toString();

				       	source += "--" + inv;
				} 
				if(coveringVertex.getInvariant() != null) {
					String inv = "**";
					if(!coveringVertex.getInvariant().toString().contains("\n"))
						inv = coveringVertex.getInvariant().toString();
					target += "--" + inv;
				}

				writer.write("\t\"" + source + "\" -> \"" + target + "\"[color=red];\n"); 
			}

			writer.write("}");
			writer.flush();
			writer.close();
		} catch (Exception exp) {
			exp.printStackTrace();
			System.exit(0);
		}
	}

	public static void printAllPaths(Queue<Vertex> queue, String fileName) {
		try{
			BufferedWriter writer = getBufferedWriter(fileName);	
			writer.write("digraph { \n");
			writer.write("\tratio=\"fill\";\n\tsize=\"8.3,11.7!\";\n\tmargin=0;\n");

			while(!queue.isEmpty()) {
				Vertex w = queue.remove();
//				System.out.println(current);
				Set<Vertex> vSet = w.getPreviousVertexSet();
				if(w.getDistance()!=0)
					writer.write("\t\"" + w.getNextVertex() + "\" -> \"" + w + "\"[label=\"" + w.getOutgoingEdge() + "\"];\n");
				for(Vertex v : vSet) { 
					queue.add(v);
				}
			}
			writer.write("}");
			writer.flush();
			writer.close();
		} catch (Exception exp) {
			exp.printStackTrace();
			System.exit(0);
		}

	}

	public static void printAllPaths2(ExceptionalUnitGraph cfg) {
		try {
			Queue<Vertex> queue = new LinkedList<Vertex>();
			Queue<Vertex> queue2 = new LinkedList<Vertex>();
			Queue<Vertex> rootSet = new LinkedList<Vertex>();
			int counter = 0;
	
			List<Unit> tails = cfg.getTails();
			for(Unit tail : tails){
				Vertex v = new Vertex();
				Edge e = new Edge(tail);
				v.addIncomingEdge(e);
				v.setLocationNumber(counter++);
				queue.add(v);
				queue2.add(v);
			}	
			
			while(!queue.isEmpty()) {
				Vertex w = queue.remove();
//				System.out.println(current);
			
				for(Edge incoming : w.getIncomingEdges()) {
					if(cfg.getUnexceptionalPredsOf(incoming.getUnit()).size() == 0) {
				
						Vertex v = new Vertex();
	
						v.setOutgoingEdge(incoming);
						v.setNextVertex(w);
						w.addPreviousVertex(v);
						v.setLocationNumber(counter++);
						v.setDistance(w.getDistance()+1);
						rootSet.add(v);
					}

					for(Unit prevEdge : cfg.getUnexceptionalPredsOf(incoming.getUnit())) {
						Vertex v = new Vertex();
						Edge e = new Edge(prevEdge);
						e.setSource(v);
						e.setTarget(w);
						
						v.setOutgoingEdge(incoming);
						v.addIncomingEdge(e);
						v.setNextVertex(w);
						w.addPreviousVertex(v);
						v.setDistance(w.getDistance()+1);
						v.setLocationNumber(counter++);
						queue.add(v);
					}	
				}	
			}

			printAllPaths(queue2, "AllPaths.dot");

			int pathNo = 0;
			while(!rootSet.isEmpty()) {
				BufferedWriter writer2 = getBufferedWriter("Path-" + ++pathNo + ".dot");	
				writer2.write("digraph { \n");
				Vertex v = rootSet.remove();
				printSinglePath(v, writer2);
				writer2.write("}");
				writer2.flush();
				writer2.close();
			}
	
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(0);
		}
	}
	
	private static void printSinglePath(Vertex v, BufferedWriter writer) throws IOException {
		if(v.getNextVertex() == null) return;

		writer.write("\t\""+v.toString()+ "\" -> \"" + v.getNextVertex().toString() + "\"[label=\"" + v.getOutgoingEdge().toString() + "\"];\n");
//		writer.write("\t\""+v.toString()+ "\" -> \"" + v.getNextVertex().toString() + "\";\n");
	
		if(v.getNextVertex() != null) printSinglePath(v.getNextVertex(), writer);
	}

	public static void printCfg(ExceptionalUnitGraph cfg) {
		
		try {
			BufferedWriter writer = getBufferedWriter("CFG.dot");	
			Queue<Unit> queue = new LinkedList<Unit>();
	
			List<Unit> tails = cfg.getTails();
			for(Unit tail : tails){
				queue.add(tail);
			}	
			
			writer.write("digraph { \n");
	
			while(!queue.isEmpty()) {
				Unit current = queue.remove();
//				System.out.println(current);
				List<Unit> predecessors = cfg.getUnexceptionalPredsOf(current);
				for(Unit predecessor : predecessors) { 
					writer.write("\t\"" + current + "\" -> \"" + predecessor + "\";\n");
					queue.add(predecessor);
				}
			}
	
			writer.write("}");
			writer.flush();
			writer.close();
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(0);

		}
	}
}

