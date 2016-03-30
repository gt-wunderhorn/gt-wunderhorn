package infoFlow;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.InterpolationContext;

import soot.Body;
import soot.toolkits.graph.ExceptionalUnitGraph;
import z3_helper.PathSolver;

public class Forest {
	private InterpolationContext ictx;
	private Map<String, Tree> subFuntionTree;
	private Tree mainFunction;
	private Tree actualFunction;
	private Stack<Tree> latestExtensionTree;
	private boolean ifProgramEnd;
	private boolean ifProgramCorrect;
	private Set<Tree> relatedTree;
	private BoolExpr[] test;
	private Tree loadFunction;
	private boolean HasLoadFunction;
	public Forest(Map<String, Body> stores, String mainFunction, String actualFunction) {
		this.ictx = new InterpolationContext();
		this.subFuntionTree = new HashMap<String, Tree>();
		this.latestExtensionTree = new Stack<Tree>();
		this.ifProgramCorrect = false;
		HelpForest helpForest = new HelpForest(stores);
		helpForest.calculateDepth();
		this.ifProgramEnd = helpForest.ifEnd();
		this.relatedTree = new HashSet<Tree>();
		Map<String, HelpTree> result = helpForest.getResult();
		this.HasLoadFunction=false;
		for (Entry<String, Body> e : stores.entrySet()) {
			System.out.println(e.getKey());
			if (e.getKey().equals(mainFunction)) {
				ExceptionalUnitGraph cfg = new ExceptionalUnitGraph(
						e.getValue());
				this.mainFunction = new Tree(cfg, this.ictx,
						result.get(mainFunction), e.getKey(), this, actualFunction);
				this.subFuntionTree.put(e.getKey(), this.mainFunction);
			} else if (e.getKey().equals(actualFunction)) {
				ExceptionalUnitGraph cfg = new ExceptionalUnitGraph(
						e.getValue());
				this.actualFunction = new Tree(cfg, this.ictx,
						result.get(actualFunction), e.getKey(), this, null);
				this.subFuntionTree.put(e.getKey(), this.actualFunction);
			} else {
				ExceptionalUnitGraph cfg = new ExceptionalUnitGraph(
						e.getValue());
				Tree subFunctionTree = new Tree(cfg, this.ictx, result.get(e
						.getKey()), e.getKey(), this, null);
				this.subFuntionTree.put(e.getKey(), subFunctionTree);
				if(e.getKey().contains("<clinit>")){
					//System.out.println("load function");
					this.loadFunction=subFunctionTree;
					this.HasLoadFunction=true;
					this.loadFunction.getNewReturnPath();
				}
			}
		}
		this.relatedTree.add(this.mainFunction);
	}

	public Tree getMainTree() {
		return this.mainFunction;
	}

	public boolean ifCorrect() {
		return this.ifProgramCorrect;

	}

	public InterpolationContext getIctx() {
		return this.ictx;
	}

	public Stack<Tree> getLatestExtensionTree() {
		return this.latestExtensionTree;
	}

	public Map<String, Tree> getSubFunctionTree() {
		return this.subFuntionTree;
	}

	public Tree getTree(String signature) {
		return this.subFuntionTree.get(signature);
	}

	public void setAllFunctionPathFullyUsed() {
		for (Entry<String, Tree> e : this.subFuntionTree.entrySet()) {
			e.getValue().setAllPathUsed(true);
		}
	}

	public void TestCorrect() {
		boolean ifAllClose = false;
		while (!ifAllClose) {
			boolean NewErrorPath = false;
			while (!NewErrorPath) {
				NewErrorPath = this.mainFunction.getNewErrorPath();
				if (NewErrorPath) {
					break;
				} else {
					this.getNewPath();
				}
			}
			ArrayList<ArrayList<Node>> errorPaths = this.mainFunction
					.getAllErrorPath();
			//System.out.println(errorPaths.size());
			PathSolver pSolver = new PathSolver(errorPaths, this);
			pSolver.CalResult();
			this.ifProgramCorrect = pSolver.ifCorrect();
			if (!ifProgramCorrect) {
				break;
			} else {
				this.test = pSolver.getTest();
				this.addAllNewReturn();
				this.cover();
				if (this.ifAllClosed()) {
					break;
				}
			}
		}
	}

	private void getNewPath() {
		for (Tree e : this.relatedTree) {
			if (e != this.mainFunction) {
				e.addAllNewReturnStmt();
				e.getNewReturnPath();
				this.addAllNewReturn();
			}
			//System.out.println(e.sizeOfPath());
		}
	}

	private boolean ifAllClosed() {
		boolean result = true;
		for (Tree e : this.relatedTree) {
			if (!e.IsTreeClose()) {
				result = false;
			}
		}
		return result;
	}

	private void cover() {
		for (Tree e : this.relatedTree) {
			e.checkTreeCover();
		}
	}

	private void addAllNewReturn() {
		for (Tree e : this.relatedTree) {
			e.addAllNewReturnStmt();
		}
	}

	public void addRelatedTree(Tree t) {
		this.relatedTree.add(t);
	}

	public BoolExpr[] getTest() {
		return this.test;
	}
	public boolean ifHasLoadFunction(){
		return this.HasLoadFunction;
	}
	public Tree getLoadFunction(){
		return this.loadFunction;
	}
	
}
