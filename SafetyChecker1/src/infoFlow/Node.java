package infoFlow;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.InterpolationContext;

import soot.Unit;
import soot.jimple.GotoStmt;
import soot.jimple.IfStmt;
import soot.jimple.InvokeStmt;
import soot.jimple.ReturnVoidStmt;
import soot.toolkits.graph.ExceptionalUnitGraph;
import z3_helper.ErrorHelper;
import z3_helper.SpecialInvoke;
import z3_helper.Z3Utility;

public class Node {
	static int NumberOfId = 0;
	// Each Node has a associate unit(statement) in SSA
	private Unit statment;
	// CHANGE STARTS
	// All successors of this node
//	private ArrayList<Node> successors;
	private ArrayList<Node> predecessors;
	// the predecessor of this node
//	private Node predecessor;
	private Node successor;
	// CHANGE ENDS
	// All nodes that are covered by this node
	private Set<Node> nodesBeCover;
	// All nodes that cover this node
	private Set<Node> CoverBy;
	// if this node is directlyCovered by some other nodes
	private boolean directlyCovered;
	// if this node's ancestor is covered by some other nodes
	private boolean ancestorCovered;
	// The invariant we get after refine
	private BoolExpr invariant;
	// Each node has an unique id
	private int id;
	// Each node has connect to tree get some overall info
	private Tree theTree;
	// Error Node
	private boolean isErrorNode;
	// Terminal Node
	private boolean isTerminal;
	// theCfg
	private ExceptionalUnitGraph cfg;
	// indicate if dummy node for invokeStmt
	private boolean ifDummy;
	// shortest path to return
	private int shortestReturn;
	// shortest unwind depth for return
	private int depthReturn;
	// show this statment is create object
	private boolean ifObject;
	// the path you choose
	private int path;
	// the path used
	private int pathUsed;
	private boolean ifUpdateIn;
	private boolean f;
	private boolean IfNonSense;
	private boolean ifSink;
	private boolean ifInput;
	private boolean IfNonSenseCompare;
	private boolean ifNew;
	private boolean ifArrayCopy;
	private boolean ifInErrorPath;
	private boolean isExpanded = false;

	//.OK.
	// static constructor to make sure each node has an unique id
	// CHANGE STARTS
//	static public Node getNewNode(Unit statement, Node predecessor, Tree theTree, ExceptionalUnitGraph cfg) {
	static public Node getNewNode(Unit statement, Node successor, Tree theTree, ExceptionalUnitGraph cfg) {
//		Node n1 = new Node(statement, predecessor, NumberOfId, theTree, cfg);
		Node n1 = new Node(statement, successor, NumberOfId, theTree, cfg);
		NumberOfId++;
		return n1;
	}
	// CHANGE ENDS

	private Node(Unit statement, Node successor /*predecessor*/, int NumberOfId,
			Tree theTree, ExceptionalUnitGraph cfg) {
		this.f = false;
		this.ifUpdateIn = false;
		this.theTree = theTree;
		this.statment = statement;
		// CHANGE STARTS
//		this.successors = new ArrayList<Node>();
//		this.predecessor = predecessor;
		this.successor = successor;
		this.predecessors =  new ArrayList<Node>();
		// CHANGE ENDS
		this.nodesBeCover = new HashSet<Node>();
		this.CoverBy = new HashSet<Node>();
		this.id = NumberOfId;
//		this.invariant = this.theTree.getIctx().mkTrue();
		this.invariant = this.theTree.getIctx().mkTrue(); //mkFalse();
		this.directlyCovered = false;
		this.ancestorCovered = false;
		this.ifDummy = false;
		this.cfg = cfg;
		this.shortestReturn = this.theTree.getHelpTree().getNodeShortest(
				statement);
		this.depthReturn = this.theTree.getHelpTree().getNodeDepth(statement);
		this.isErrorNode = ErrorHelper.ifError(statement);
		if(this.isErrorNode || (this.successor != null && this.successor.getIfInErrorPath())) {
			this.ifInErrorPath = true;
		}
		// if this stmt doesn't have any successors, then it is terminated.
		// CHANGE STARTS
//		List<Unit> next = this.cfg.getUnexceptionalSuccsOf(statement);
		List<Unit> previous = this.cfg.getUnexceptionalPredsOf(statement);
//		if (next.size() != 0) {
		if (previous.size() != 0) {
			this.isTerminal = false;
		} else {
			this.isTerminal = true;
		}
		// CHANGE ENDS
		this.ifObject = ErrorHelper.ifObject(statement);
		this.path = 0;
		this.pathUsed = 0;
		this.IfNonSense = SpecialInvoke.ifNosenseInvoke(
				this.theTree.getForest(), statement);
		this.ifSink = SpecialInvoke.ifSinkInvoke(statement);
		this.ifInput = SpecialInvoke.ifInput(statement);
		this.IfNonSenseCompare = SpecialInvoke.ifNonSenseCompare(statement);
		this.ifNew = SpecialInvoke.ifNew(statement, this.theTree.getForest());
		this.ifArrayCopy = SpecialInvoke.ifArrayCopy(statement);
	}
	
	//.OK.	
	public void setShortestReturn(int n) {
		this.shortestReturn = n;
	}

	//.OK.	
	public int getShortestReturn() {
		return this.shortestReturn;
	}

	//.OK.	
	public void setDummy() {
		this.ifDummy = true;
	}

	//.OK.	
	public boolean ifDummy() {
		return this.ifDummy;
	}

	//.OK.	
	public void clearInvariant() {
		//CHANGE STARTS
//		this.invariant = this.theTree.getIctx().mkTrue();
		this.invariant = this.theTree.getIctx().mkFalse();
		//CHANGE ENDS
	}

	//.OK.	
	public void addInvariant(BoolExpr newInvariant) {
		BoolExpr currentInvariant = this.invariant;
		//CHANGE STARTS
		this.invariant = this.theTree.getIctx().mkAnd(currentInvariant, newInvariant);
//		this.invariant = this.theTree.getIctx().mkOr(currentInvariant, newInvariant);
		//CHANGE ENDS
		this.ifUpdateIn = true;
		// System.out.println("Statement:"+this.statment+",Invariant"+newInvariant+"node id"+this.id);
	}

	//.OK.
	// get predecessor
	public Node successor() {
		return this.successor;
	}
	public void updateSuccessor(Node successor) {
		this.successor = successor;
	}
//	public Node predecessor() {
//		return this.predecessor;
//	}

	//.OK.
	// get id
	public int getId() {
		return this.id;
	}

	//.OK.
	// update invariant
	public void updateInvariant(BoolExpr invariant) {
		this.invariant = invariant;
	}

	//.OK.
	// return invariant
	public BoolExpr getInvariant() {
		return this.invariant;
	}

	//.OK.
	// return unit
	public Unit getStmt() {
		return this.statment;
	}

	// check if this node is covered
	public boolean IsCovered() {
		if (this.directlyCovered || this.ancestorCovered) {
			return true;
		} else {
			return false;
		}
	}

	public boolean isAncestorCovered() {
		return this.ancestorCovered;
	}

	// check if it is covered by other node, or it is covered because its
	// ancestor is covered
	public boolean hasCoverNode() {
		if (this.CoverBy.size() != 0) {
			return true;
		} else {
			return false;
		}
	}

	// check if this node is covered by the other node
	public boolean IfCoveredBy(Node n2) {
		BoolExpr n1Invariant = this.invariant;
		BoolExpr n2Invariant = n2.getInvariant();
		return Z3Utility.checkEntail(n1Invariant, n2Invariant,
				this.theTree.getIctx());
	}

	// set this node is covered by some node
	public void setBeCovered(Node n2) {
		this.CoverBy.add(n2);
		n2.addCover(this);
		// since if this CoverBy other nodes, it cannot cover other nodes.
		if (this.CoverBy.size() == 1) {
			this.clearCover();
			this.directlyCovered = true;
			if (!this.ancestorCovered) {
				this.setDescendantCover();
			}
		}
	}

	public void setDescendantCoverTrue() {
		this.ancestorCovered = true;
	}

	public void setDescendantCoverFalse() {
		this.ancestorCovered = false;
	}

	public void setDescendantCover() {
		//CHANGE FOR DELETED
//		for (Node successor : this.successors) {
		if(successor != null) {
			this.clearCover();
			successor.setDescendantCover();
			successor.setDescendantCoverTrue();
		}
	}

	// set n2 is covered by this node
	private void addCover(Node n2) {
		this.nodesBeCover.add(n2);
	}

	// set this node is not covered by an node
	public void removeCoverRelation(Node e) {
		this.CoverBy.remove(e);
		if (this.CoverBy.size() == 0) {
			this.directlyCovered = false;
			if (!this.ancestorCovered) {
				this.setDescendantUncover();
			}
		}
	}

	//.OK.???
	// set all descendant uncover
	public void setDescendantUncover() {
		// CHANGE FOR DELETED
//		for (Node successor : this.successors) {
			successor.setDescendantCoverFalse();
			successor.setDescendantUncover();
//		}
	}

	//.OK.???
	// clear all cover relation, and remove the corresponding relation in be
	// covered nodes
	public void clearCover() {
		for (Node n : this.nodesBeCover) {
			n.removeCoverRelation(this);
		}
		this.nodesBeCover.clear();
	}

	//.OK.
	// check if this node is leaf
	public boolean ifLeaf() {
		// CHANGE ONE LINE
//		return this.successors.size() == 0;
		return this.predecessors.size() == 0;
	}

	//.OK.
	// check if Error Node
	public boolean ifError() {
		return this.isErrorNode;
	}

	//.OK.
	// return all the successor nodes
//	public ArrayList<Node> successors() {
//		return this.successors;
//	}
	public ArrayList<Node> predecessors() {
		return this.predecessors;
	}
	public boolean removePredecessor(Node predecessor) {
		return predecessors.remove(predecessor);
	}

	//.OK.???
	// return the whole path
	public ArrayList<Node> returnPath() {
		ArrayList<Node> path = new ArrayList<Node>();
		Node current = this;
		while (current != null) {
			path.add(current);
			// CHANGE ONE LINE
//			current = current.predecessor;
			current = current.successor;
		}
		// CHANGE REMOVED ONE LINE COS NO NEED ANYMORE
//		Collections.reverse(path);
		return path;
	}

	// .OK.??? return may be need
//	public boolean ifReturn() {
//		return this.isTerminal;
//	}
	public boolean ifRoot() {
		return this.isTerminal;
	}

	//.OK.??? May be need to change to get path
	// return the unique to path to this Error Node,not adding error node
	public ArrayList<Node> path() {
		ArrayList<Node> path = new ArrayList<Node>();
		Node current = this;
		// CHANGE ONE LINE
//		while (current != null) {
		while (current != null) {
			path.add(current);
			if(current.ifError()) break;
			// CHANGE ONE LINE
//			current = current.predecessor;
			current = current.successor;
		}
		
//		Collections.reverse(path);
		return path;
	}

	//.OK.
//	public void addSuccessor(Node n2) {
//		this.successors.add(n2);
//	}
	public void addPredecessor(Node n2) {
		this.predecessors.add(n2);
	}

	//.OK.
	// Expand the successors
	public void expand() {
		ExceptionalUnitGraph cfg = this.theTree.getCfg();
		// CHANGE STARTS
//		List<Unit> nextUnits = cfg.getUnexceptionalSuccsOf(this.statment);
		List<Unit> prevUnits = cfg.getUnexceptionalPredsOf(this.statment);
			
		for (Unit u1 : prevUnits) {
			Node child = Node.getNewNode(u1, this, this.theTree, this.cfg);
			this.predecessors.add(child);
		}
		// CHANGE ENDS
	}

	//.OK.???
	// after refine, we want to clear all cover relation
	public void clearAllCover() {
		this.nodesBeCover = new HashSet<Node>();
		this.CoverBy = new HashSet<Node>();
		this.directlyCovered = false;
		this.ancestorCovered = false;
		// CHANGE STARTS
//		for (Node successor : this.successors) {
//			successor.clearAllCover();
//		}
		for (Node predecessor : this.predecessors) {
			predecessor.clearAllCover();
		}
		// CHANGE ENDS
	}

	//.OK.
	public String toString() {
		return this.statment.toString()  + "--" + this.ifDummy() + "\n";
	}

	//.OK.
	public int getDepth() {
		return this.depthReturn;
	}

	//.OK.
	public int getShortest() {
		return this.shortestReturn;
	}

	//.OK.
	public boolean ifObject() {
		return this.ifObject;
	}

	//.OK.
	public Set<Node> getCover() {
		return this.CoverBy;
	}

	//.OK.
	public void setPath(int i) {
		this.path = i;
	}

	//.OK.
	public int getPath() {
		return this.path;
	}

	//.OK.
	public void setPathUsed(int i) {
		this.pathUsed = i;
	}

	//.OK.
	public int getPathUsed() {
		return this.pathUsed;
	}

	//.OK.
	public boolean getIfUpdate() {
		return this.ifUpdateIn;
	}

	//.OK.???
//	public void setInterpolant() {
//		this.f = true;
//	}
	public void clearInterpolant() {
		this.f = false;
	}

	//.OK.
	public boolean ifGenerateInterpolant() {
		return this.f;
	}

	//.OK.
	public boolean getIfNonsense() {
		return this.IfNonSense;
	}

	//.OK.
	public boolean getIfSink() {
		return this.ifSink;
	}

	//.OK.
	public boolean getIfInput() {
		return this.ifInput;
	}

	//.OK.
	public boolean getIfNonsenseCompare() {
		return this.IfNonSenseCompare;
	}

	//.OK.
	public boolean getIfNew() {
		return this.ifNew;
	}

	//.OK.
	public boolean getIfArryCopy() {
		return this.ifArrayCopy;
	}

	//.OK.
	public boolean getIfInErrorPath() { return this.ifInErrorPath; }
	//.OK.
	public void setIfInErrorPath(boolean b) { this.ifInErrorPath = b; }


	public void setIsExpanded(boolean b) { this.isExpanded = b; }
	public boolean getIsExpanded() { return this.isExpanded; }
}
