package safetyChecker;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;

import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Log;

import dotty.CfgConverter;

import safetyChecker.exception.ErrorLocationNotFoundException;
import safetyChecker.exception.MainFunctionNotFoundException;

import soot.Body;
import soot.Unit;
import soot.toolkits.graph.ExceptionalUnitGraph;

public class ProgramTree {

	private Vertex returnLeaf;
	private Set<Vertex> liSet;
	private Vertex lf;
	private Map<String, Body> stores;
	// for algorithm
	private Set<Vertex> vertexSet = new HashSet<Vertex>();
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
		this.itpHandler = new InterpolationHandler(this.ictx, this.z3Handler);
		this.coverRelation = new CoverRelation(this.ictx);
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
			this.returnLeaf = new Vertex();
			this.returnLeaf.setReturnLocation(true);
			
			for(int i = 0; i < cfg.getTails().size(); i++) {	
				Edge e = new Edge(cfg.getTails().get(i));
				this.returnLeaf.addIncomingEdge(e);
			}
			this.uncovered.add(this.returnLeaf);

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
				LogUtils.infoln("HUHU");
				System.exit(0);
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

	private void unwind() throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		LogUtils.infoln("----->Unwind");

		boolean windingDone = false;

		while(!this.uncovered.isEmpty()) {
			Vertex v = uncovered.remove();
			LogUtils.debugln(v.getOutgoingEdge() + "---" + v + "--" + coverRelation.isCovered(v));
			if(coverRelation.isCovered(v)) continue;

			boolean errorPathFound = expandBFS(v);
			LogUtils.debugln("expandBFs is done");
			
			if(errorRootSet.size() >  2000) {
			 	LogUtils.fatalln("Error Root Size has reached to 2000 and stopped manually");      
				break;
			}

			if(!errorRootQueue.isEmpty()) {
				LogUtils.debugln("errorRootQueue = " + errorRootQueue);
				Vertex errorRoot = errorRootQueue.remove(); 

				LogUtils.fatalln("error root # = " + errorRootSet.size());
				z3Handler.convertPathtoZ3Script(errorRoot); 
				errorLocationFeasible = itpHandler.createInterpolant(errorRoot);
				LogUtils.debugln("printing result path");
				printResult(errorRoot.toString());

				if(errorLocationFeasible) break;

				LogUtils.debugln("updatecover is calling");
				coverRelation.updateCover();
				LogUtils.debugln("updateCover is done");
			}
		}	
		Queue<Vertex> q = new LinkedList<Vertex>();
		q.add(returnLeaf);		
		LogUtils.debugln("coveringVertexMap # " + coverRelation.getCoveringMap().size());
		LogUtils.debugln("coveredVertexMap # " + coverRelation.getCoveredByMap().size());
		LogUtils.debugln("unitVertexMap # " + coverRelation.getUnitVertexMap().size());
		for(Entry<Unit, LinkedList<Vertex>> entry : coverRelation.getUnitVertexMap().entrySet()) {
			LogUtils.debugln(entry.getKey() + "--" + entry.getValue());
			LogUtils.debugln("-------------");
		}
		CfgConverter.printAllPaths(q, "_all.dot");
		CfgConverter.printErrorPaths(errorSet, "_errors.dot", coverRelation);
	}

	private boolean expandBFS(Vertex w) throws MainFunctionNotFoundException, ErrorLocationNotFoundException {
		LogUtils.debugln("----->expand : " + w + "--" + w.getOutgoingEdge() + "--" + coverRelation.isCovered(w));
//		if(w.getOutgoingEdge() != null)	LogUtils.warningln(w.getOutgoingEdge().isInErrorPath());

		boolean result = false;
		if (!coverRelation.isCovered(w)) {
			LogUtils.debugln("if (!coverRelation.isCovered(w))---" + w.getOutgoingEdge()); 

			for (Edge incomingEdge : w.getIncomingEdges()) {
				Vertex v = new Vertex();
				v.setOutgoingEdge(incomingEdge);
				incomingEdge.setSource(v);
				if(incomingEdge.isControlLocation())
					coverRelation.updateUnitVertexMap(incomingEdge);

				v.setNextVertex(w);
				v.setDistance(w.getDistance()+1);
				v.setLocationNumber(++locationCounter);
				w.addPreviousVertex(v);
				this.vertexSet.add(v);

				if(!incomingEdge.isInErrorPath() && !errorSet.isEmpty())
					continue;	
				this.uncovered.add(v);

				if(cfg.getUnexceptionalPredsOf(incomingEdge.getUnit()).size() == 0) {
					v.setHeadLocation(true);
//					v.setInvariant(itpHandler.getTrueInvariant());
					if(incomingEdge.isInErrorPath()) { 
						errorRootSet.add(v);
						errorRootQueue.add(v);
						result = true;
					} else { 
						returnRootQueue.add(v);
						result = true;		
					}
				}
				
				for (Unit action : cfg.getPredsOf(incomingEdge.getUnit())) {
					Edge e = new Edge(action);
					e.setTarget(v);
					e.setProgramTree(this);
					v.addIncomingEdge(e);
					unitController.analyzeEdge(e, stores);
//					if(e.isControlLocation()) 
//						coverRelation.updateUnitVertexMap(e);

					if(e.isSubFunction()) {
						subFunctionList.add(e);
//						e.getProgramTree().getNewReturnPath();
					}
					

					if(e.isErrorEdge())
						errorSet.add(v);
				}

				if(v.getOutgoingEdge().isErrorEdge()) {
					this.uncovered.clear();
					this.uncovered.add(v);
					continue;
				}
			}
		}
		LogUtils.debugln("<-----expand : w.incomingEdge#" + w.getIncomingEdges().size() + " : w.previousVertexSet#" + w.getPreviousVertexSet().size());
		return result;
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
 
}
