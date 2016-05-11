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
	private Stack<Tree> latestExtensionTree;
	private boolean ifProgramEnd;
	private boolean ifProgramCorrect;
	private Set<Tree> relatedTree;
	private BoolExpr[] test;
	private Tree loadFunction;
	private boolean HasLoadFunction;

	public Forest(Map<String, Body> stores, String mainFunction) {
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
			if (e.getKey().equals(mainFunction)) {
				ExceptionalUnitGraph cfg = new ExceptionalUnitGraph(
						e.getValue());
				this.mainFunction = new Tree(cfg, this.ictx,
						result.get(mainFunction), e.getKey(), this);
				this.subFuntionTree.put(e.getKey(), this.mainFunction);
			} else {
				ExceptionalUnitGraph cfg = new ExceptionalUnitGraph(
						e.getValue());
				Tree subFunctionTree = new Tree(cfg, this.ictx, result.get(e
						.getKey()), 
						e.getKey(), this);
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

	//.OK.
	public Tree getMainTree() {
		return this.mainFunction;
	}

	//.OK.
	public boolean ifCorrect() {
		return this.ifProgramCorrect;

	}

	//.OK.
	public InterpolationContext getIctx() {
		return this.ictx;
	}

	//.OK.
	public Stack<Tree> getLatestExtensionTree() {
		return this.latestExtensionTree;
	}

	//.OK.
	public Map<String, Tree> getSubFunctionTree() {
		return this.subFuntionTree;
	}

	//.OK.
	public Tree getTree(String signature) {
		return this.subFuntionTree.get(signature);
	}

	//.OK.
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
				System.out.println(this.mainFunction);
				NewErrorPath = this.mainFunction.getNewErrorPath();
				if (NewErrorPath) {
					break;
				} else {
					this.getNewPath();
				}
			}
			ArrayList<ArrayList<Node>> errorPaths = this.mainFunction
					.getAllErrorPath();
//			System.out.println(errorPaths);
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

	//.OK.
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

	//.OK.
	public void addRelatedTree(Tree t) {
		this.relatedTree.add(t);
	}

	//.OK.
	public BoolExpr[] getTest() {
		return this.test;
	}

	//.OK.
	public boolean ifHasLoadFunction(){
		return this.HasLoadFunction;
	}

	//.OK.
	public Tree getLoadFunction(){
		return this.loadFunction;
	}
	
}
