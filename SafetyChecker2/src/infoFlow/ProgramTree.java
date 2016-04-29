package infoFlow;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;

import infoFlow.exception.ErrorLocationNotFoundException;
import infoFlow.exception.MainFunctionNotFoundException;

import soot.Body;
import soot.Unit;
import soot.toolkits.graph.ExceptionalUnitGraph;

public class ProgramTree {

	// private Set<ProgramLocation> locations;
	// private Set<Action> actions;
	private Vertex returnLeaf;
	private Set<Vertex> liSet;
	private Vertex lf;
	private Map<String, Body> stores;
	// for algorithm
	private Set<Vertex> vertexSet = new HashSet<Vertex>();
	private Set<Edge> edgeSet = new HashSet<Edge>();
	private Stack<Edge> path = new Stack<Edge>();

	// from me
	private InterpolationHandler iHandler;
	private boolean programCorrect = false;
	private HashMap<String, ProgramTree> calleeFunctions;
	private HashSet<Vertex> rootSet;
	private LinkedList<LinkedList<Vertex>> returnPaths;
	private LinkedList<LinkedList<Vertex>> errorPaths;
	private String mainFunction;
	private String actualFunction;
	private ExceptionalUnitGraph cfg;
	private HelpTree helpTree;
	private String signature;

	public ProgramTree(Map<String, Body> stores, String mainFunction, String actualFunction)
			throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		this.iHandler = new InterpolationHandler();
		this.programCorrect = false;
		this.calleeFunctions = new HashMap<String, ProgramTree>();
		this.rootSet = new HashSet<Vertex>();
		this.returnPaths = new LinkedList<LinkedList<Vertex>>();
		this.errorPaths = new LinkedList<LinkedList<Vertex>>();
		this.mainFunction = mainFunction;
		this.actualFunction = actualFunction;
		this.stores = stores;

		boolean mainFunctoionFound = findMainFunction();

		if (mainFunctoionFound)
			startTest();
		else
			throw new MainFunctionNotFoundException(this.mainFunction + " does not exist in the current program");
	}

	private void startTest() throws ErrorLocationNotFoundException {
		boolean errorLocationFound = findErrorLocation(returnLeaf);
		if (!errorLocationFound)
			throw new ErrorLocationNotFoundException(this.mainFunction + " does not have any ErrorLocation");
		// start unwind process
		unwind();
		printTree(0, returnLeaf);
		printTree();
	}

	private void printTree() {
		LogUtils.debugln("************");
		for(Vertex v2 : rootSet) {
			LogUtils.debugln(v2.getOutgoingEdge());
		}
	}

	private void printTree(int indent, Vertex v) {
		for(Edge e : v.getIncomingEdges()) {
			for(int i = 0; i < indent; i++) LogUtils.debug("^");
			LogUtils.debugln(e.getUnit());
		}

		for(Vertex p : v.getPreviousVertexSet()) {
			printTree(indent+1, p);
		}
	}

	private boolean findErrorLocation(Vertex w) {
		for (Edge incomingEdge : w.getIncomingEdges()) {
			for (Unit incomingUnit : cfg.getPredsOf(incomingEdge.getUnit())) {
				LogUtils.debugln("findErrorLocation : " + incomingUnit);
				if (UnitController.isErrorUnit(incomingUnit)) {
					this.lf = new Vertex();
					lf.setErrorLocation(true);
					lf.setNextVertex(w);
					Edge e = new Edge(incomingUnit);
					lf.addIncomingEdge(e);
					w.addPreviousVertex(lf);

					return true;
				} else {
					LogUtils.debugln("else girdi");
					for (Unit u2 : cfg.getPredsOf(incomingUnit)) {
						Vertex v2 = new Vertex();
						Edge e = new Edge(u2);
						v2.addIncomingEdge(e);
					}
				}
			}
		}
		return false;
	}

	private boolean findMainFunction() {
		for (Entry<String, Body> entry : stores.entrySet()) {
			if (entry.getKey().equals(mainFunction)) {
				this.cfg = new ExceptionalUnitGraph(entry.getValue());
				this.helpTree = new HelpTree(cfg);
				this.signature = entry.getKey();
				LogUtils.debugln("findMainFunction : " + signature);
				// Assumption is that we have only one ErrorLocation and return
				// point
				// if we have multiple returns, may be we should have multiple
				// trees.
				this.returnLeaf = new Vertex();
				this.returnLeaf.setReturnLocation(true);

				Edge e = new Edge(cfg.getTails().get(0));
				this.returnLeaf.addIncomingEdge(e);

				return true;
			}
		}
		return false;
	}

	private Stack<Vertex> uncovered = new Stack<Vertex>();
	private Set<Vertex> covered = new HashSet<Vertex>();
	private Map<Vertex, Vertex> coveringRelation = new HashMap<Vertex, Vertex>();
	private void unwind() {
		uncovered.add(returnLeaf);
		while(!uncovered.isEmpty()) {
			Vertex v = uncovered.pop();
			LogUtils.detailln("unwind" + v.getIncomingEdges());
//			for(Vertex v : uncovered) {
//				for(Edge e : v.getIncomingEdges()) {
//					for(Unit u : cfg.getUnexceptionalPredsOf(v.get)) {
//						close();
//					}
//				}	
//			}
			DFS(v);			
		}		
	}

	private void DFS(Vertex v) {
		LogUtils.detailln("----->DFS : v.incomingEdges=" + v.getIncomingEdges() + " : v.previousVertexSet=" + v.getPreviousVertexSet().size()); 
		close(v);				
		if(!v.isCovered()) {
			if(v.isHeadLocation()) {
				refine(v);
				// for all w <= v : close(w)
			}
			expand(v);
			for(Vertex previousVertex : v.getPreviousVertexSet()) { 
				DFS(previousVertex);
			}	
		}
	}

	private void close(Vertex v) {}

	private void expand(Vertex w) {
		LogUtils.detailln("----->expand : w.incomingEdge#" + w.getIncomingEdges().size() + " : w.previousVertexSet#" + w.getPreviousVertexSet().size());

		if (!w.isCovered()) {
			for (Edge incomingEdge : w.getIncomingEdges()) {
				if(cfg.getUnexceptionalPredsOf(incomingEdge.getUnit()).size() == 0) {
			
					LogUtils.detail("HUHUHUHUHUHUHUHU");
					Vertex v = new Vertex();

					v.setOutgoingEdge(incomingEdge);
					v.setHeadLocation(true);
					v.setNextVertex(w);
					w.addPreviousVertex(v);
					this.vertexSet.add(v);
					
					uncovered.push(v);
					rootSet.add(v);
				}

				for (Unit action : cfg.getPredsOf(incomingEdge.getUnit())) {
					Vertex v = new Vertex();
					Edge e = new Edge(action);

					e.setV(v);
					e.setW(w);

					v.setOutgoingEdge(incomingEdge);
					v.addIncomingEdge(e);
					v.setNextVertex(w);
					w.addPreviousVertex(v);
					this.vertexSet.add(v);

					//uncovered.push(v);
				}
			}
		}
		LogUtils.detailln("<-----expand : w.incomingEdge#" + w.getIncomingEdges().size() + " : w.previousVertexSet#" + w.getPreviousVertexSet().size());
	}

	private void refine(Vertex v) {
		// if(v.isHeadLocation() && !v.isSigh()) {
		// List<Edge> path = createUniquePathPi(v);
		// if(iHandler.createInterpolation()) {
		//
		// }
		// }
	}

	private void cover(Vertex v, Vertex w) {

	}

	public List<Edge> createUniquePathPi(Vertex v) {

		return null;
	}

	// public Set<Vertex> getVertexSet() { return this.vertexSet; }
	// public Set<Edge> getEdgeSet() { return this.edgeSet; }
}
