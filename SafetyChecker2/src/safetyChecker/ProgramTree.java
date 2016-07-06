package safetyChecker;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;

import com.microsoft.z3.InterpolationContext;

import safetyChecker.exception.ErrorLocationNotFoundException;
import safetyChecker.exception.MainFunctionNotFoundException;
import safetyChecker.utilities.DottyConverter;
import safetyChecker.utilities.LogUtils;
import safetyChecker.z3ScriptManager.Z3ScriptHandler;

import soot.Body;
import soot.Unit;
import soot.toolkits.graph.ExceptionalUnitGraph;

public class ProgramTree {

	private Vertex root;
	private Set<Vertex> liSet;
	private Vertex lf;
	private Map<String, Body> stores;
	// for algorithm
	private Set<Edge> edgeSet = new HashSet<Edge>();
	private Stack<Edge> path = new Stack<Edge>();

	// from me
	private InterpolationContext ictx;
	private Z3ScriptHandler z3Handler; 
	private InterpolationHandler itpHandler;

	private boolean errorLocationFeasible = false;
	private boolean mainFunction;
	private boolean treeClosed;
	private LinkedList<Edge> subFunctionList;
	private HashSet<Vertex> errorRootSet;
	private Queue<Vertex> errorRootQueue;
	private Queue<Vertex> returnRootQueue;
	private LinkedList<LinkedList<Vertex>> returnPaths;
	private LinkedList<LinkedList<Vertex>> errorPaths;
	private String functionSignature;
	private String functionName;
	private static Map<String, Integer> functionNameInvokeCount = new HashMap<String, Integer>();
	private ExceptionalUnitGraph cfg;
	//private HelpTree helpTree;
	private String signature;
	private UnitController unitController;
	private CoverRelation coverRelation;

	private Queue<Vertex> uncovered = new LinkedList<Vertex>();
	private Queue<Vertex> errorSet = new LinkedList<Vertex>();
	private int locationCounter = 0;

	public ProgramTree(Map<String, Body> stores, String functionSignature, boolean mainFunction) throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		LogUtils.detailln("------->ProgramTree");
		this.errorLocationFeasible = false;
		this.subFunctionList = new LinkedList<Edge>();
		this.errorRootSet = new HashSet<Vertex>();
		this.errorRootQueue = new LinkedList<Vertex>();
		this.returnRootQueue = new LinkedList<Vertex>();
		this.returnPaths = new LinkedList<LinkedList<Vertex>>();
		this.errorPaths = new LinkedList<LinkedList<Vertex>>();
		this.functionSignature = functionSignature;
		this.stores = stores;
		this.unitController = new UnitController();
		this.mainFunction = mainFunction;
		this.ictx = new InterpolationContext();
		this.z3Handler = new Z3ScriptHandler(this.ictx);
		this.coverRelation = new CoverRelation(this.ictx, this);
		this.itpHandler = new InterpolationHandler(this.ictx, this.z3Handler, this.coverRelation);
		if(this.mainFunction)
			LogUtils.detailln("mainFunction = " + functionSignature);
		else
			LogUtils.detailln("subFunction = " + functionSignature);

		boolean mainFunctionFound = findMainFunction();
		if (mainFunctionFound && this.mainFunction)
			startTest();
		else if(this.mainFunction)
			throw new MainFunctionNotFoundException(this.functionSignature + " does not exist in the current program");
	}

	private void startTest() throws ErrorLocationNotFoundException, MainFunctionNotFoundException {
		LogUtils.detailln("------------>startTest()");

		unwind();
		LogUtils.detailln("<-------------startTest");
	}

	private boolean findErrorLocation(Vertex w) {
		for (Edge incomingEdge : w.getIncomingEdges()) {
			for (Unit incomingUnit : cfg.getPredsOf(incomingEdge.getUnit())) {
				LogUtils.debugln("findErrorLocation : " + incomingUnit);
				if (unitController.isErrorUnit(incomingUnit)) {
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
		if(stores.containsKey(functionSignature)) {
			this.cfg = new ExceptionalUnitGraph(stores.get(functionSignature));//entry.getValue()
			
			Body body = stores.get(functionSignature);
		 	this.functionName = body.getMethod().getName();
			if(functionNameInvokeCount.containsKey(functionName))
				functionNameInvokeCount.put(functionName,functionNameInvokeCount.get(functionName)+1);
			else
				functionNameInvokeCount.put(functionName, 0);	
			this.signature = functionSignature;
	
			// Assumption is that we have only one ErrorLocation and return  point
			// if we have multiple returns, may be we should have multiple trees.
			this.root = new Vertex();
			for(int i = 0; i < cfg.getTails().size(); i++) {	
				Edge returnEdge = new Edge(cfg.getTails().get(i));
				returnEdge.setReturnEdge(true);
				boolean unDoneFlag = (cfg.getUnexceptionalPredsOf(returnEdge.getUnit()).size() > 1) ? true : false; 
				Vertex returnVertex = this.addVertex(root, returnEdge, unDoneFlag);
				returnVertex.setReturnLocation(true);
//				this.unDoneLeaves.add(returnVertex);
			}
			return true;
		}
		return false;
	}

	public boolean getNewReturnPath() throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		LogUtils.warningln(">>>>>>>> ProgramTree.getNewReturnPath");
		while(!this.uncovered.isEmpty()) {
			Vertex v = uncovered.remove();
			LogUtils.detailln(v.getIncomingEdges());
			boolean returnPathFound = expandBFS(v);
			if(returnRootQueue.size() > 0) {
				Vertex returnRoot = returnRootQueue.peek();
				z3Handler.convertPathtoZ3Script(returnRoot);
				return true;
			}
		}
		LogUtils.warningln("<<<<<<<<< ProgramTree.getNewReturnPath");
		return false;
	}

	public Vertex getNewReturnRoot() {
		if(!returnRootQueue.isEmpty()) {
			Vertex returnRoot = returnRootQueue.remove();
			LogUtils.warningln("returnRoot=" + returnRoot);
		//	z3Handler.convertPathtoZ3Script(returnRoot);
			return returnRoot;
		}
		return null;
	}

	public boolean isTreeDone() { 
		boolean result = true;
		if(!uncovered.isEmpty()) {
			
		}
		return result;
	}

	private HashMap<Vertex, Vertex> candidate2BeInPath = new HashMap<Vertex, Vertex>();
	private HashMap<Vertex, Vertex> treeConnection = new HashMap<Vertex, Vertex>();
//	private Queue<Vertex> unDoneLeaves = new LinkedList<Vertex>();

	private void unwind() throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		LogUtils.debugln("----->Unwind");

		boolean windingDone = false;

		while(!this.uncovered.isEmpty()) {
			try {
				Vertex v = uncovered.remove();
				if(this.isConnectionCovered(v))
					continue;
	
				LogUtils.debugln(v.getOutgoingEdge() + "---" + v + "--" + coverRelation.isCovered(v));
				if(coverRelation.isCovered(v)) continue;
	
				boolean errorPathFound = expandBFS(v);
				if(!errorRootQueue.isEmpty()) {
					LogUtils.debugln("errorRootQueue = " + errorRootQueue);
					Vertex errorRoot = errorRootQueue.remove(); 
					
					LogUtils.infoln("error root #" + errorRootSet.size() + "=" + errorRoot);
					z3Handler.convertPathtoZ3Script(errorRoot); 
					errorLocationFeasible = itpHandler.createInterpolant(errorRoot);
					LogUtils.debugln("printing result path");
					this.printResult(errorRoot.toString());
	
					if(errorLocationFeasible) break;
					coverRelation.updateCover();
				}
			} catch (Exception ex) {
				LogUtils.warningln("Exception occured during unwind");
			}
		}	

		try {
			Queue<Vertex> q = new LinkedList<Vertex>();
			q.add(root);		
	
			DottyConverter.printAllPaths(q, "_all.dot");
			LogUtils.warningln("errorSet size = " + errorSet.size());
			DottyConverter.printErrorPaths(errorSet, "_errors.dot", coverRelation);
			LogUtils.debugln("<------unwind");
		} catch (Exception ex) {
			LogUtils.warningln("Error in printig tree");
		}
	}

	private boolean expandBFS(Vertex w) throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		LogUtils.debugln("----->expand : " + w + "--" + w.getOutgoingEdge() + "--" + coverRelation.isCovered(w) + "**" + w.getOutgoingEdge().isInErrorPath());

		boolean result = false;
		if (!coverRelation.isCovered(w)) {
			LogUtils.debugln("if (!coverRelation.isCovered(w))---" + w.getOutgoingEdge()); 
			List<Unit> unitList = cfg.getUnexceptionalPredsOf(w.getOutgoingEdge().getUnit());
			boolean unDoneFlag = false;
			if(unitList.size() > 1)
				unDoneFlag = true;;
			for (Unit unit : unitList) {
				
				Edge edge = new Edge(unit);
				edge.setProgramTree(this);
				edge.setInErrorPath(w.getOutgoingEdge().isInErrorPath());
				Vertex v = addVertex(w, edge, unDoneFlag);

				unitController.analyzeEdge(edge, stores, cfg);
				if(!edge.isInErrorPath() && !errorSet.isEmpty()) continue;
				if(edge.isControlLocation()) coverRelation.updateUnitVertexMap(v);
				if(edge.isErrorEdge()) errorSet.add(w);
				if(edge.isSubFunction()) subFunctionList.add(edge);
				if(edge.isEntryLocation()) {
					if(edge.isInErrorPath()) {
						this.addErrorEntryLocation(v);
						result = true;
					} else {
						this.returnRootQueue.add(v);
						result = false;
					}
				}
				
				if(v.getOutgoingEdge().isErrorEdge()) {
//					this.unDoneLeaves.clear();
					this.treeConnection.clear();
					this.candidate2BeInPath.clear();
					this.uncovered.clear();
					this.uncovered.add(v);

					v.setNextVertex(w);
					w.addPreviousVertex(v);
					edge.setTarget(w);
					w.addIncomingEdge(edge);

					continue;
				}
			}
		}
		LogUtils.debugln("<-----expand : w.incomingEdge#" + w.getIncomingEdges().size() + " : w.previousVertexSet#" + w.getPreviousVertexSet().size());
		return result;
	}

	private boolean isConnectionCovered(Vertex vertex) {
		while(this.candidate2BeInPath.containsKey(vertex)) {
			Vertex connection = this.candidate2BeInPath.get(vertex);
			Vertex unDoneLeaf = this.treeConnection.get(connection);
			
			if(coverRelation.isCovered(unDoneLeaf)) return true;
			vertex = unDoneLeaf;
		}	
		return false;
	}

	private void addErrorEntryLocation(Vertex entryVertex) {
		this.errorRootSet.add(entryVertex);
		this.errorRootQueue.add(entryVertex);
		
		while(this.candidate2BeInPath.containsKey(entryVertex)) {
			Vertex connection = this.candidate2BeInPath.get(entryVertex);
			Vertex undoneLeaf = this.treeConnection.get(connection);
			
			connection.setNextVertex(undoneLeaf);
			connection.getOutgoingEdge().setTarget(undoneLeaf);
	
			undoneLeaf.addIncomingEdge(connection.getOutgoingEdge());
			undoneLeaf.addPreviousVertex(connection);
	
//			int noOfCurrentPreds = undoneLeaf.getPreviousVertexSet().size();
//			int noOfMaxPreds = this.cfg.getUnexceptionalPredsOf(undoneLeaf.getOutgoingEdge().getUnit()).size();

//			if(noOfCurrentPreds == noOfMaxPreds)
//				this.unDoneLeaves.remove(undoneLeaf);
//			if(noOfCurrentPreds < noOfMaxPreds)
//				this.uncovered.add(undoneLeaf);
			
			entryVertex = undoneLeaf;
		}
	}	

	private Vertex addVertex(Vertex nextVertex, Edge edge, boolean unDoneFlag) {
		Vertex prevVertex = new Vertex();
		prevVertex.setOutgoingEdge(edge);

		if(!unDoneFlag) {
			prevVertex.setNextVertex(nextVertex);
	
			nextVertex.addIncomingEdge(edge);
			nextVertex.addPreviousVertex(prevVertex);
	
			edge.setTarget(nextVertex);

			if(this.candidate2BeInPath.containsKey(nextVertex)) {
				Vertex nv = this.candidate2BeInPath.get(nextVertex);
				this.candidate2BeInPath.put(prevVertex, nv);
				this.candidate2BeInPath.remove(nextVertex);
			}
		} else { 
//			this.unDoneLeaves.add(nextVertex);
			this.treeConnection.put(prevVertex, nextVertex);
			this.candidate2BeInPath.put(prevVertex, prevVertex);
		}

		edge.setSource(prevVertex);
		prevVertex.setDistance(nextVertex.getDistance()+1);
		this.uncovered.add(prevVertex);
		prevVertex.setLocationNumber(++locationCounter);
		return prevVertex;

	}

	private void refine(Vertex v) {
	}

	public String getProgramDefinition() {
		return "_" + this.functionName + "_" + ProgramTree.functionNameInvokeCount.get(this.functionName);
	}	

	public String getFunctionName() {
		return this.functionName;
	}

	public void printResult(String function) {
		LogUtils.printResult(function, errorLocationFeasible);
	}

	protected Queue<Vertex> getUncovered() { return this.uncovered; }
	protected ExceptionalUnitGraph getCfg() { return this.cfg; }
 
}
