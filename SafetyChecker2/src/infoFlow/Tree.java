package infoFlow;

import java.util.ArrayList;
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
	private Node root;
	private Node exceptionNode;
	private Forest theForest;
	private Set<Node> dummyCollection;

	public Tree(ExceptionalUnitGraph cfg, InterpolationContext ictx,
			HelpTree helpTree, String signature, Forest f1, String actualFunction) {
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
		this.root = Node
				.getNewNode(cfg.getHeads().get(0), null, this, this.cfg);
		//this.exceptionNode = Node.getNewNode(cfg.getUnexceptionalPredsOf(cfg.getTails().get(0), null, this, this.cfg);
		this.Leaf.add(root);
		
		this.dummyCollection = new HashSet<Node>();
		this.DummyUnitNodeCollection = new HashMap<Unit, ArrayList<Node>>();
	}

	public HelpTree getHelpTree() {
		return this.helpTree;
	}

	public InterpolationContext getIctx() {
		return this.ictx;
	}

	public Node getRoot() {
		return this.root;
	}

	public ArrayList<ArrayList<Node>> getAllErrorPath() {
		return this.errorPaths;
	}

	public boolean IsTreeClose() {
		return this.treeClosed;
	}

	public int sizeOfPath() {
		return this.returnPaths.size();
	}

	public ArrayList<Node> getPath(int i) {
		return this.returnPaths.get(i);
	}

	public void setAllPathUsed(boolean used) {
		this.allPathUsed = used;
	}

	public boolean isAllPathUsed() {
		return this.allPathUsed;
	}

	public boolean getNewErrorPath() {
		this.treeClosed = false;
		Comparator<Node> nodeComparator = new NodeComparator();
		Queue<Node> expandQueue = new PriorityQueue<Node>(nodeComparator);
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
			if (currentNode.ifDummy()) {
				String signature = HelpTree.getMethodSignature(currentUnit);
				Tree nextTree = this.theForest.getTree(signature);
				if (nextTree.sizeOfPath() == 0) {
					nextTree.getNewReturnPath();
					this.theForest.addRelatedTree(nextTree);
				}
				int size = nextTree.sizeOfPath();
				currentNode.setPathUsed(size);
				for (int i = 0; i < size; i++) {
					Node returnNode = Node.getNewNode(currentUnit, currentNode,
							this, this.cfg);
					currentNode.addSuccessor(returnNode);
					returnNode.setPath(i);
					this.updateNodeCollection(returnNode);
					expandQueue.add(returnNode);
					this.Leaf.add(returnNode);
				}
				continue;
			}
			ArrayList<Node> successors = currentNode.successors();
			for (int i = 0; i < successors.size(); i++) {
				Node successor = successors.get(i);
				Unit theUnit = successor.getStmt();
				// if this stmt is invoke, we want to add a dummy node
				if ((HelpTree.isInvoke(theUnit)) && (!successor.ifError())) {
					if (!successor.getIfNonsense()) {
						successor.setDummy();
					}
				}
				// according to different Unit, put all nodes into map
				this.updateNodeCollection(successor);
				if (successor.ifError()) {
					ArrayList<Node> errorPath = successor.path();
					//System.out.println("the new error path");
					PathCoverter.printPath(errorPath);
					this.errorPaths.add(errorPath);
					getNewPath = true;
				} else {
					expandQueue.add(successor);
					this.Leaf.add(successor);
				}
			}
		}
		return getNewPath;
	}

	public boolean getNewReturnPath() {
		this.treeClosed = false;
		Comparator<Node> nodeComparator = new NodeComparator();
		Queue<Node> expandQueue = new PriorityQueue<Node>(nodeComparator);
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
					Node returnNode = Node.getNewNode(currentUnit, currentNode,
							this, this.cfg);
					currentNode.addSuccessor(returnNode);
					returnNode.setPath(i);
					this.updateNodeCollection(returnNode);
					expandQueue.add(returnNode);
					this.Leaf.add(returnNode);
				}
				continue;
			}
			ArrayList<Node> successors = currentNode.successors();
			for (int i = 0; i < successors.size(); i++) {
				Node successor = successors.get(i);
				Unit theUnit = successor.getStmt();
				// if this stmt is invoke, we want to add a dummy node
				if ((HelpTree.isInvoke(theUnit)) && (!successor.ifError())) {
					if (!successor.getIfNonsense()) {
						successor.setDummy();
					}
				}
				// according to different Unit, put all nodes into map
				this.updateNodeCollection(successor);
				if (successor.ifReturn()) {
					ArrayList<Node> returnPath = successor.path();
					this.returnPaths.add(returnPath);
					getNewPath = true;
				} else {
					expandQueue.add(successor);
					this.Leaf.add(successor);
				}
			}
		}
		return getNewPath;
	}

	// check if tree cover.
	public void checkTreeCover() {
		// clear all cover relation
		this.root.clearAllCover();
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

	public ExceptionalUnitGraph getCfg() {
		return this.cfg;
	}

	public String getSignature() {
		return this.signature;
	}

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
				n.addSuccessor(returnNode);
				returnNode.setPath(i);
				this.updateNodeCollection(returnNode);
				this.Leaf.add(returnNode);
			}
			n.setPathUsed(sizeOfPath);
		}
	}

	public Forest getForest() {
		return this.theForest;
	}
}
