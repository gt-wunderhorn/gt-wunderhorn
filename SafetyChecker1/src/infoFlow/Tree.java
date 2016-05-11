package infoFlow;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;

import com.microsoft.z3.InterpolationContext;

import soot.Unit;
import soot.Value;
import soot.jimple.AssignStmt;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;
import soot.toolkits.graph.ExceptionalUnitGraph;

import z3_helper.PathCoverter;
import z3_helper.PathHelper;
import z3_helper.PathSolver;
import z3_helper.SpecialInvoke;

public class Tree {
	private ExceptionalUnitGraph cfg;
	private ArrayList<ArrayList<Node>> returnPaths;
	private Set<Node> Leaf;
	private InterpolationContext ictx;
	private Map<Unit, ArrayList<Node>> UnitNodeCollection;
	private Map<Unit, ArrayList<Node>> DummyUnitNodeCollection;
	private ArrayList<ArrayList<Node>> errorPaths;
	private boolean treeClosed;
	private boolean allPathUsed;
	private HelpTree helpTree;
	private String signature;
	// CHANGE STARTS
//	private Node root;
	private Node error;
	private Node returnNode;
	// CHANGE ENDS
	private Forest theForest;
	private Set<Node> dummyCollection;
	private String actualFunction;

	//.OK.
	public Tree(ExceptionalUnitGraph cfg, InterpolationContext ictx, HelpTree helpTree, String signature, Forest f1) {
		this.cfg = cfg;
		this.Leaf = new HashSet<Node>();
		this.returnPaths = new ArrayList<ArrayList<Node>>();
		this.ictx = ictx;
		this.UnitNodeCollection = new HashMap<Unit, ArrayList<Node>>();
		this.treeClosed = true;
		this.allPathUsed = true;
		this.errorPaths = new ArrayList<ArrayList<Node>>();
		this.helpTree = helpTree;
		this.signature = signature;
		this.theForest = f1;

		// for here, we assume the heads will only be one unit, if we handle
		// procedure we might
		// need to change here
// 		CHANGE STARTS
//		this.root = Node.getNewNode(cfg.getHeads().get(0), null, this, this.cfg);
//		this.Leaf.add(root);
		this.returnNode = Node.getNewNode(cfg.getTails().get(0), null, this, this.cfg);
		this.Leaf.add(returnNode);
//		CHANGE ENDS
		this.dummyCollection = new HashSet<Node>();
		this.DummyUnitNodeCollection = new HashMap<Unit, ArrayList<Node>>();
	}

	//.OK.
	public HelpTree getHelpTree() {
		return this.helpTree;
	}

	//.OK.
	public InterpolationContext getIctx() {
		return this.ictx;
	}

	//.OK.
	// CHANGE STARTS
	public Node getReturnNode() {
		return this.returnNode;
	}
//	public Node getRoot() {
//		return this.root;
//	}
	// CHANGE ENDS

	//.OK.
	public ArrayList<ArrayList<Node>> getAllErrorPath() {
		return this.errorPaths;
	}

	//.OK.
	public boolean IsTreeClose() {
		return this.treeClosed;
	}

	//.OK.
	public int sizeOfPath() {
		return this.returnPaths.size();
	}

	//.OK.
	public ArrayList<Node> getPath(int i) {
		return this.returnPaths.get(i);
	}

	//.OK.
	public void setAllPathUsed(boolean used) {
		this.allPathUsed = used;
	}

	//.OK.
	public boolean isAllPathUsed() {
		return this.allPathUsed;
	}

	public boolean getNewErrorPath() {
		this.treeClosed = false;
		Comparator<Node> nodeComparator = new NodeComparator();
		Queue<Node> expandQueue = new PriorityQueue<Node>(100,nodeComparator);
		for (Node leaf : this.Leaf) {
			if (!leaf.IsCovered()) {
				expandQueue.add(leaf);
			}
		}
		if (expandQueue.isEmpty()) {
			this.treeClosed = true;
		}
		boolean getNewPath = false;
		while ((!expandQueue.isEmpty()) && (!getNewPath)) {
			Node currentNode = expandQueue.remove();
			currentNode.expand();
			this.Leaf.remove(currentNode);
			Unit currentUnit = currentNode.getStmt();
//			System.out.println(currentUnit + "--" + currentNode.ifDummy());
			if (currentNode.ifDummy() && !currentNode.getIsExpanded()) {
				
				String signature = HelpTree.getMethodSignature(currentUnit);
				Tree nextTree = this.theForest.getTree(signature);
				if (nextTree.sizeOfPath() == 0) {
					nextTree.getNewReturnPath();
					this.theForest.addRelatedTree(nextTree);
				}
				int size = nextTree.sizeOfPath();
				currentNode.setPathUsed(size);
				for (int i = 0; i < size; i++) {
					Node suc = currentNode.successor();
					if(!suc.removePredecessor(currentNode)) {
						System.out.println(currentNode + " cannot be deleted from " + suc);
						System.exit(0);
					}
//					Node returnNode = Node.getNewNode(currentUnit, currentNode, this, this.cfg);
					Node returnNode = Node.getNewNode(currentUnit, suc, this, this.cfg);
//					currentNode.addSuccessor(returnNode);
					returnNode.predecessors().add(currentNode);
					currentNode.updateSuccessor(returnNode);
					returnNode.setPath(i);
					this.updateNodeCollection(returnNode);
//					expandQueue.add(returnNode);
//					this.Leaf.add(returnNode);
				}
//				continue;
			}
			//CHANGE STARTS
//			ArrayList<Node> successors = currentNode.successors();
			ArrayList<Node> predecessors = currentNode.predecessors();
//			for (int i = 0; i < successors.size(); i++) {
			for (int i = 0; i < predecessors.size(); i++) {
//				Node successor = successors.get(i);
				Node predecessor = predecessors.get(i);
				Unit theUnit = predecessor.getStmt();
				// if this stmt is invoke, we want to add a dummy node
				if ((HelpTree.isInvoke(theUnit)) && (!predecessor.ifError())) {
					if (!predecessor.getIfNonsense()) {
						predecessor.setDummy();
					}
				}
				// according to different Unit, put all nodes into map
				this.updateNodeCollection(predecessor);
//				if (predecessor.ifError()) {
				if (predecessor.ifRoot() && predecessor.getIfInErrorPath()) {
					ArrayList<Node> errorPath = predecessor.path();
					//System.out.println("the new error path");
					this.errorPaths.add(errorPath);
					getNewPath = true;
				} else {
					expandQueue.add(predecessor);
					this.Leaf.add(predecessor);
				}
			}
			//CHANGE ENDS
		}
		return getNewPath;
	}

	public boolean getNewReturnPath() {
		this.treeClosed = false;
		Comparator<Node> nodeComparator = new NodeComparator();
		Queue<Node> expandQueue = new PriorityQueue<Node>(100,nodeComparator);
		for (Node leaf : this.Leaf) {
			if (!leaf.isAncestorCovered()) {
				expandQueue.add(leaf);
			}
		}
		if (expandQueue.isEmpty()) {
			this.treeClosed = true;
		}
		boolean getNewPath = false;
		while ((!expandQueue.isEmpty()) && (!getNewPath)) {
			Node currentNode = expandQueue.remove();
			currentNode.expand();
			this.Leaf.remove(currentNode);
			Unit currentUnit = currentNode.getStmt();
			if (currentNode.ifDummy()) {
				System.out.println("in ifDummy");
				System.out.println("Tree.getNewReturnPath");
				System.exit(0);
				currentNode.setDummy();
				String signature = HelpTree.getMethodSignature(currentUnit);
				Tree nextTree = this.theForest.getTree(signature);
				if (nextTree.sizeOfPath() == 0) {
					nextTree.getNewReturnPath();
					this.theForest.addRelatedTree(nextTree);
				}
				int size = nextTree.sizeOfPath();
				currentNode.setPathUsed(size);
				for (int i = 0; i < size; i++) {
//					Node returnNode = Node.getNewNode(currentUnit, currentNode, this, this.cfg);
					Node rootNode = Node.getNewNode(currentUnit, currentNode, this, this.cfg);
//					currentNode.addSuccessor(returnNode);
					currentNode.addPredecessor(rootNode);
					rootNode.setPath(i);
					this.updateNodeCollection(rootNode);
					expandQueue.add(rootNode);
					this.Leaf.add(rootNode);

				}
				continue;
			}
//			ArrayList<Node> successors = currentNode.successors();
			ArrayList<Node> predecessors = currentNode.predecessors();
			for (int i = 0; i < predecessors.size(); i++) {
//				Node successor = successors.get(i);
				Node predecessor = predecessors.get(i);
				Unit theUnit = predecessor.getStmt();
				// if this stmt is invoke, we want to add a dummy node
				if ((HelpTree.isInvoke(theUnit)) && (!predecessor.ifError())) {
					if (!predecessor.getIfNonsense()) {
						predecessor.setDummy();
					}
				}
				// according to different Unit, put all nodes into map
				this.updateNodeCollection(predecessor);
//				if (predecessor.ifReturn()) {
				if (predecessor.ifRoot()) {
					ArrayList<Node> returnPath = predecessor.path();
					this.returnPaths.add(returnPath);
					getNewPath = true;
				} else {
					expandQueue.add(predecessor);
					this.Leaf.add(predecessor);
				}
			}
		}
		return getNewPath;
	}

	// check if tree cover.
	public void checkTreeCover() {
		// clear all cover relation
		// CHANGE STARTS
//		this.root.clearAllCover();
		this.returnNode.clearAllCover();
		// CHANGE ENDS
		this.checkCover();
		boolean treeClose = true;
		for (Node n : this.Leaf) {
			/** System.out.println("Something is improtant" + n.getStmt()); **/
			if (!n.IsCovered()) {
				treeClose = false;
			}
		}
		this.treeClosed = treeClose;
	}

	// add all cover relation
	public void checkCover() {
		for (Entry<Unit, ArrayList<Node>> e1 : this.UnitNodeCollection
				.entrySet()) {
			ArrayList<Node> similarNodes = e1.getValue();
			for (int i = similarNodes.size() - 1; i > -1; i--) {
				Node checkNode = similarNodes.get(i);
				for (int j = 0; j < i; j++) {
					boolean ifCover = checkNode
							.IfCoveredBy(similarNodes.get(j));
					if (ifCover) {
						boolean isAncestorCovered = similarNodes.get(j)
								.isAncestorCovered();
						// in here, we use aggressive strategy to adding cover
						// relation,
						// however , if the node ancestor is covered, the this
						// node won't cover any other node
						if (!isAncestorCovered) {
							checkNode.setBeCovered(similarNodes.get(j));
							/**
							 * System.out.println("cover is working");
							 * System.out.println(checkNode.getStmt());
							 **/
						}
					}
				}
			}
		}
		for (Entry<Unit, ArrayList<Node>> e1 : this.DummyUnitNodeCollection
				.entrySet()) {
			ArrayList<Node> similarNodes = e1.getValue();
			for (int i = similarNodes.size() - 1; i > -1; i--) {
				Node checkNode = similarNodes.get(i);
				for (int j = 0; j < i; j++) {
					boolean ifCover = checkNode
							.IfCoveredBy(similarNodes.get(j));
					if (ifCover) {
						boolean isAncestorCovered = similarNodes.get(j)
								.isAncestorCovered();
						// in here, we use aggressive strategy to adding cover
						// relation,
						// however , if the node ancestor is covered, the this
						// node won't cover any other node
						if (!isAncestorCovered) {
							checkNode.setBeCovered(similarNodes.get(j));
							/**
							 * System.out.println("cover is working");
							 * System.out.println(checkNode.getStmt());
							 **/
						}
					}
				}
			}
		}
	}

	//.OK.
	public ExceptionalUnitGraph getCfg() {
		return this.cfg;
	}

	//.OK.
	public String getSignature() {
		return this.signature;
	}

	//.OK.
	public Set<Node> getLeaf() {
		return this.Leaf;
	}

	private void updateNodeCollection(Node n) {
		if (!n.ifDummy()) {
			Unit theUnit = n.getStmt();
			if (this.UnitNodeCollection.containsKey(theUnit)) {
				ArrayList<Node> similarNode = this.UnitNodeCollection
						.remove(theUnit);
				similarNode.add(n);
				this.UnitNodeCollection.put(theUnit, similarNode);
			} else {
				ArrayList<Node> similarNode = new ArrayList<Node>();
				similarNode.add(n);
				this.UnitNodeCollection.put(theUnit, similarNode);
			}
		} else {
			this.dummyCollection.add(n);
			Unit theUnit = n.getStmt();
			if (this.DummyUnitNodeCollection.containsKey(theUnit)) {
				ArrayList<Node> similarNode = this.DummyUnitNodeCollection
						.remove(theUnit);
				similarNode.add(n);
				this.DummyUnitNodeCollection.put(theUnit, similarNode);
			} else {
				ArrayList<Node> similarNode = new ArrayList<Node>();
				similarNode.add(n);
				this.DummyUnitNodeCollection.put(theUnit, similarNode);
			}
		}
	}

	public void addAllNewReturnStmt() {
		for (Node n : this.dummyCollection) {
			int pathUsed = n.getPathUsed();
			Unit u = n.getStmt();
			String signature = HelpTree.getMethodSignature(u);
			int sizeOfPath = this.theForest.getTree(signature).sizeOfPath();
			for (int i = pathUsed; i < sizeOfPath; i++) {
				Node returnNode = Node.getNewNode(u, n, this, this.cfg);
				System.out.println("****");
				System.out.println(n.predecessors());
				System.out.println(n);
				System.out.println(n.successor());
				System.out.println(returnNode);
				System.out.print("Tree.addAllNewReturnStmt");
				System.exit(0);
				// CHANGE ONE LINE
//				n.addSuccessor(returnNode);
				n.addPredecessor(returnNode);
				returnNode.setPath(i);
				this.updateNodeCollection(returnNode);
				this.Leaf.add(returnNode);
			}
			n.setPathUsed(sizeOfPath);
		}
	}

	//.OK.
	public Forest getForest() {
		return this.theForest;
	}

	public String getActualFunction() { return this.actualFunction; }
	public void setActualFunction(String actualFunction) { this.actualFunction = actualFunction; }

}
