package z3_helper;

import infoFlow.Forest;
import infoFlow.HelpTree;
import infoFlow.Node;
import infoFlow.Tree;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Stack;

import soot.Unit;
import soot.Value;
import soot.jimple.AssignStmt;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;

import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

public class PathCoverter {
	// index of guide
	private int GuideIndex;
	// latest global value (Object and Field)
	private Map<String, Expr> latestGlobal;
	// used for sort get Id
	private Map<String, NewSort> sortId;
	// used for get z3 sort
	private Map<String, Sort> sortMap;
	// the number of copy
	private int Copy;
	// the number of path
	private int numberofPath;
	// rename string
	private String renameString;
	// ictx
	private InterpolationContext ictx;
	// z3 covert result
	private ArrayList<ArrayList<BoolExpr>> result;
	// z3 substitute
	private Map<String, String> substitute;
	// z3 subsittute Sort
	private Map<String, Sort> substituteSort;
	// for new arrayObject
	private int arrayObject;
	// global arrayName
	private Map<String, Integer> arrayName;
	// handle array updating
	private Map<String, Integer> realArraySize;
	// handle pass parameter
	private Stack<Expr> parameters;
	// handle return
	private Stack<Expr> returnPara;
	// path
	private ArrayList<Node> path;
	// function tree
	private Map<String, Tree> subFunctionTree;
	// stores the result
	private Map<Integer, Expr> errorCheck;
	// string count
	private int stringCount;
	// covertCount
	private int covertCount;
	private Expr[] origianl;
	private Expr[] to;
	private int inputSensitive;
	private ArrayList<Expr> sensitiveInput;
	private Map<String, Expr> staticField;
	private Forest theForest;

	public PathCoverter(int PathNumber, int Copy, InterpolationContext ictx,
			ArrayList<Node> path, Map<String, Tree> subFuntionTree, Forest f1) {
		this.Copy = Copy;
		this.errorCheck = new HashMap<Integer, Expr>();
		this.latestGlobal = new HashMap<String, Expr>();
		this.sortId = new HashMap<String, NewSort>();
		this.sortMap = new HashMap<String, Sort>();
		this.substitute = new HashMap<String, String>();
		this.substituteSort = new HashMap<String, Sort>();
		this.arrayName = new HashMap<String, Integer>();
		this.realArraySize = new HashMap<String, Integer>();
		this.ictx = ictx;
		this.GuideIndex = 1;
		this.numberofPath = PathNumber;
		this.result = new ArrayList<ArrayList<BoolExpr>>();
		this.renameString = "Path" + this.numberofPath + "Copy" + Copy;
		this.arrayObject = 0;
		this.parameters = new Stack<Expr>();
		this.returnPara = new Stack<Expr>();
		this.path = path;
		this.subFunctionTree = subFuntionTree;
		this.stringCount = 0;
		this.inputSensitive = 1;
		this.sensitiveInput = new ArrayList<Expr>();
		this.staticField = new HashMap<String, Expr>();
		this.theForest = f1;
	}

	public BoolExpr getInterpolant() {
		this.covertCount = 0;
		return generateInterpolant(this.path);
	}

	public int addInvariant(int start, BoolExpr[] invariant) {
		this.generateRename();
		return updateInvariant(this.path, start, invariant);
	}

	private void generateRename() {
		Expr[] original = new Expr[this.substitute.size()];
		Expr[] to = new Expr[this.substitute.size()];
		int i = 0;
		for (Entry<String, String> e : this.substitute.entrySet()) {
			Sort s = this.substituteSort.get(e.getKey());
			if (s == null) {
				s = this.ictx.getIntSort();
			}
			original[i] = this.ictx.mkConst(e.getKey(), s);
			to[i] = this.ictx.mkConst(e.getValue(), s);
			i++;
		}
		this.to = to;
		this.origianl = original;
	}

	private int updateInvariant(ArrayList<Node> path, int start,
			BoolExpr[] invariant) {
		boolean ifReturn = false;
		String methodSignature = "";
		int index = start;
		for (int i = 1; i < path.size(); i++) {
			Node n = path.get(i);
			Unit u = n.getStmt();
			if (n.ifError()) {
				BoolExpr theInvariant = invariant[index];
				theInvariant = (BoolExpr) theInvariant.substitute(
						this.origianl, this.to);
				n.addInvariant(theInvariant);
				index++;
				continue;
			}
			if (n.ifDummy()) {
				BoolExpr theInvariant = invariant[index];
//				System.out.println("theInvariant1" + theInvariant);
				theInvariant = (BoolExpr) theInvariant.substitute(
						this.origianl, this.to);
				methodSignature = HelpTree.getMethodSignature(u);
				ifReturn = true;
//				System.out.println(i + "--" + n);
//				System.out.println("theInvariant2"+theInvariant);
//				System.out.println("PathCoverter.updateInvariant");
//				System.exit(0);
				n.addInvariant(theInvariant);
				index++;
				Node next = path.get(i + 1);
				BoolExpr nextInvairant = invariant[index];
				nextInvairant = (BoolExpr) nextInvairant.substitute(
						this.origianl, this.to);
				next.addInvariant(nextInvairant);
				index++;
			} else {
				if (ifReturn) {
					ifReturn = false;
					Tree calleeProcedure = this.subFunctionTree
							.get(methodSignature);
					ArrayList<Node> nextPath = calleeProcedure.getPath(n
							.getPath());
					int nextStart = index;
					index = this
							.updateInvariant(nextPath, nextStart, invariant);
					BoolExpr theInvariant = invariant[index];
					// System.out.println("return path"+theInvariant);
					theInvariant = (BoolExpr) theInvariant.substitute(
							this.origianl, this.to);
					n.addInvariant(theInvariant);
					index++;
				} else {
					BoolExpr theInvariant = invariant[index];
					theInvariant = (BoolExpr) theInvariant.substitute(
							this.origianl, this.to);
					n.addInvariant(theInvariant);
					index++;
				}
			}
		}
		return index;
	}

	public static void printPath(ArrayList<Node> path) {
		String methodSignature = "";
		boolean ifReturn = false;
		for (int i = 1; i < path.size(); i++) {
			Node n = path.get(i);
			// CHANGE ONE LINE
//			n.setInterpolant();
			n.clearInterpolant();
			Unit u = n.getStmt();
			if (n.ifError()) {
				continue;
			}
			if (n.ifDummy()) {
				if (u instanceof AssignStmt) {
					Value right = ((AssignStmt) u).getRightOp();
					methodSignature = ((InvokeExpr) right).getMethod()
							.getSignature();
				} else {
					methodSignature = ((InvokeStmt) u).getInvokeExpr()
							.getMethod().getSignature();
				}
				ifReturn = true;
			} else {
				if (ifReturn) {
					ifReturn = false;
					//System.out.println(n.getPath());
				} else {
				}
			}
		}
	}

	private BoolExpr generateInterpolant(ArrayList<Node> path) {
		ArrayList<BoolExpr> result = this.result.get(this.covertCount);

		/*System.out.println("--------------------------------------------");
		for (int i = 0; i < result.size(); i++) {
			System.out.println(result.get(i));
			if (i < path.size()) {
				System.out.println(path.get(i).getStmt());
			}
		}
		System.out.println("--------------------------------------------");*/
		/**
		 * System.out.println(path.get(0).getStmt());
		 * System.out.println("the size of interpolant" + result.size());
		 * System.out.println("the size of path" + path.size());
		 **/
		this.covertCount++;
		boolean ifReturn = false;
		String methodSignature = "";
		BoolExpr f1 = result.get(0);
		BoolExpr interResult = ictx.MkInterpolant(f1);
		for (int i = 1; i < path.size(); i++) {
			Node n = path.get(i);
			// CHANGE ONE LINE
//			n.setInterpolant();
			n.clearInterpolant();
			Unit u = n.getStmt();
			if (n.ifError()) {
				continue;
			}
			if (n.ifDummy()) {
				if (u instanceof AssignStmt) {
					Value right = ((AssignStmt) u).getRightOp();
					methodSignature = ((InvokeExpr) right).getMethod()
							.getSignature();
				} else {
					methodSignature = ((InvokeStmt) u).getInvokeExpr()
							.getMethod().getSignature();
				}
				ifReturn = true;
				BoolExpr thisFormula = result.get(i);
				BoolExpr middleOr = this.ictx.mkOr(thisFormula, interResult);
				interResult = ictx.MkInterpolant(middleOr);
			} else {
				if (ifReturn) {
					ifReturn = false;
					Tree calleeProcedure = this.subFunctionTree
							.get(methodSignature);
					ArrayList<Node> nextPath = calleeProcedure.getPath(n
							.getPath());
					BoolExpr lowLevel = generateInterpolant(nextPath);
					BoolExpr returnPolicy = result.get(i);
					BoolExpr andFormula = ictx.mkOr(interResult, lowLevel,
							returnPolicy);
					interResult = ictx.MkInterpolant(andFormula);
				} else {
					if (n.getIfNew()) {
						Tree loadFunction = this.theForest.getLoadFunction();
						ArrayList<Node> loadPah = loadFunction.getPath(0);
						BoolExpr lowLevel = generateInterpolant(loadPah);
						BoolExpr thisFormula = result.get(i);
						BoolExpr andFormula = ictx.mkOr(interResult, lowLevel,
								thisFormula);
						interResult = ictx.MkInterpolant(andFormula);
					} else {
						BoolExpr thisFormula = result.get(i);
						BoolExpr middleOr = this.ictx.mkOr(thisFormula,
								interResult);
						interResult = ictx.MkInterpolant(middleOr);
					}
				}
			}
		}
		return interResult;
	}

	public boolean hasGlobal(String name) {
		if (this.latestGlobal.containsKey(name)) {
			return true;
		} else {
			return false;
		}
	}

	public Expr getGlobal(String name) {
		return this.latestGlobal.get(name);
	}

	public void updateGlobal(String name, Expr value) {
		if (this.latestGlobal.containsKey(name)) {
			this.latestGlobal.remove(name);
			this.latestGlobal.put(name, value);
		} else {
			this.latestGlobal.put(name, value);
		}
	}

	public Map<String, NewSort> getSortId() {
		return this.sortId;
	}

	public Map<String, Sort> getSort() {
		return this.sortMap;
	}

	public InterpolationContext getIctx() {
		return this.ictx;
	}

	public void storeResult(ArrayList<BoolExpr> result) {
		this.result.add(result);
	}

	public String getRenameString(int level) {
		String result = this.renameString + "Level" + level;
		return result;
	}

	public void addSubstitute(String newName, String oldName) {
		this.substitute.put(newName, oldName);
	}

	public int getLevel() {
		int level = this.GuideIndex;
		level--;
		this.GuideIndex++;
		return level;
	}

	public String getNameOfArrayObject() {
		String result = this.renameString + "ArrayObject" + this.arrayObject;
		this.arrayObject++;
		return result;
	}

	public String getGlobalName(String name) {
		if (this.arrayName.containsKey(name)) {
			int index = this.arrayName.get(name);
			index++;
			String result = this.renameString + "GlobalArray" + name + index;
			this.arrayName.remove(name);
			this.arrayName.put(name, index);
			return result;
		} else {
			int index = 1;
			String result = "GlobalArray" + name + index;
			this.arrayName.put(name, index);
			return result;
		}
	}

	public int getRealArraySize(String name) {
		if (this.realArraySize.containsKey(name)) {
			int size = this.realArraySize.get(name);
			size++;
			this.realArraySize.remove(name);
			this.realArraySize.put(name, size);
			return size;
		} else {
			int size = 1;
			this.realArraySize.put(name, size);
			return size;

		}
	}

	public void pushPara(Expr e) {
		this.parameters.push(e);
	}

	public Expr popPara() {
		return this.parameters.pop();
	}

	public void pushReturn(Expr e) {
		this.returnPara.push(e);
	}

	public boolean ifParaEmpty() {
		if (this.parameters.isEmpty()) {
			return true;
		}
		return false;
	}

	public Expr popReturn() {
		return this.returnPara.pop();
	}

	public void updateSubstituteSort(String newName, Sort s) {
		this.substituteSort.put(newName, s);
	}

	// for testing
	public ArrayList<ArrayList<BoolExpr>> getCoverReesult() {
		return this.result;
	}

	public void updateErrorCheck(int i, Expr rightZ3) {
		this.errorCheck.put(i, rightZ3);
	}

	public Map<Integer, Expr> getErrorCheck() {
		return this.errorCheck;
	}

	public String getStringName() {
		String s = this.renameString + "String" + this.stringCount;
		stringCount++;
		return s;
	}

	public void pushSensitiveInput(Expr theString) {
		this.sensitiveInput.add(theString);
	}

	public ArrayList<Expr> returnSensitive() {
		return this.sensitiveInput;
	}

	public int getPathNumber() {
		return this.numberofPath;
	}

	public void updateStaticField(String name, Expr z3) {
		if (this.staticField.containsKey(name)) {
			this.staticField.remove(name);
			this.staticField.put(name, z3);
		} else {
			this.staticField.put(name, z3);
		}
	}

	public Expr getStaticField(String name) {
		return this.staticField.get(name);
	}
}
