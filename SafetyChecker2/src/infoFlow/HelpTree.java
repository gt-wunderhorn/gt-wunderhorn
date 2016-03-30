package infoFlow;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;

import soot.Unit;
import soot.Value;
import soot.JastAddJ.ThisAccess;
import soot.jimple.AssignStmt;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;
import soot.jimple.ReturnStmt;
import soot.jimple.ReturnVoidStmt;
import soot.toolkits.graph.ExceptionalUnitGraph;

public class HelpTree {
	private ExceptionalUnitGraph cfg;
	private Map<Unit, Integer> shortestPath;
	private Map<Unit, Integer> lowestUnwind;
	private boolean findReturn;
	private ArrayList<Unit> returnUnit;
	private Unit root;
	private Map<String, HelpTree> stores;
	private boolean ifModified;

	public HelpTree(ExceptionalUnitGraph cfg) {
		this.cfg = cfg;
		this.shortestPath = new HashMap<Unit, Integer>();
		this.lowestUnwind = new HashMap<Unit, Integer>();
		this.ifModified = false;
		this.findReturn = false;
		this.returnUnit = new ArrayList<Unit>();
		this.root = this.cfg.getHeads().get(0);
		Set<Unit> visited = new HashSet<Unit>();
		visited.add(this.root);
		Queue<Unit> expandQueue = new LinkedList<Unit>();
		expandQueue.add(this.root);
		this.findReturnUnit(visited, expandQueue);
		this.ShortestReturnPath();
		this.UpdateAllZeros();
	}

	public void setStores(Map<String, HelpTree> stores) {
		this.stores = stores;
	}

	public void getDepth() {
		this.ifModified = false;
		ArrayList<Unit> path = new ArrayList<Unit>();
		for (int i = 0; i < this.returnUnit.size(); i++) {
			path.add(this.returnUnit.get(i));
			getDepth1(path, 0);
		}
	}

	private void getDepth1(ArrayList<Unit> path, int depth) {
		Unit curretOne = path.get((path.size() - 1));
		List<Unit> preds = this.cfg.getUnexceptionalPredsOf(curretOne);
		for (int i = 0; i < preds.size(); i++) {
			int newDepth = depth;
			Unit p = preds.get(i);
			if (!path.contains(p)) {
				if (IfNonAnalysis(p)) {
					this.updateLowest(p, newDepth);
					ArrayList<Unit> newPath = new ArrayList<Unit>();
					newPath.addAll(path);
					newPath.add(p);
					getDepth1(newPath, newDepth);
				} else {
					String signature = getMethodSignature(p);
					HelpTree subFunction = this.stores.get(signature);
					if (subFunction.IfRootHasDepth()) {
						newDepth = depth + subFunction.getRootDepth() + 1;
						this.updateLowest(p, newDepth);
						ArrayList<Unit> newPath = new ArrayList<Unit>();
						newPath.addAll(path);
						newPath.add(p);
						getDepth1(newPath, newDepth);
					}
				}
			}
		}
	}

	private void ShortestReturnPath() {
		for (int j = 0; j < this.returnUnit.size(); j++) {
			Unit returnUnit = this.returnUnit.get(j);
			Queue<Unit> expandQueue = new LinkedList<Unit>();
			expandQueue.add(returnUnit);
			this.updateShortest(returnUnit, 0);
			Set<Unit> allUnits = this.getAllUnites(returnUnit);
			Set<Unit> visited = new HashSet<Unit>();
			while (!allUnits.isEmpty()) {
				Unit e = expandQueue.remove();
				allUnits.remove(e);
				List<Unit> predes = this.cfg.getUnexceptionalPredsOf(e);
				int length = this.shortestPath.get(e);
				length++;
				for (int i = 0; i < predes.size(); i++) {
					Unit p = predes.get(i);
					if (!visited.contains(p)) {
						this.updateShortest(p, length);
						expandQueue.add(p);
						visited.add(p);
					}
				}
			}
		}
	}

	private Set<Unit> getAllUnites(Unit returnUnit) {
		Set<Unit> allUnits = new HashSet<Unit>();
		allUnits.add(returnUnit);
		Queue<Unit> expandQueue = new LinkedList<Unit>();
		expandQueue.add(returnUnit);
		while (!expandQueue.isEmpty()) {
			Unit e = expandQueue.remove();
			List<Unit> predes = this.cfg.getUnexceptionalPredsOf(e);
			for (int j = 0; j < predes.size(); j++) {
				Unit p = predes.get(j);
				if (!allUnits.contains(p)) {
					allUnits.add(p);
					expandQueue.add(p);
				}
			}
		}
		return allUnits;
	}

	private Set<Unit> getAllDepthZeroUnites() {
		Set<Unit> allUnits = new HashSet<Unit>();
		for (int i = 0; i < this.returnUnit.size(); i++) {
			allUnits.add(this.returnUnit.get(i));
			Queue<Unit> expandQueue = new LinkedList<Unit>();
			expandQueue.add(this.returnUnit.get(i));
			while (!expandQueue.isEmpty()) {
				Unit e = expandQueue.remove();
				List<Unit> predes = this.cfg.getUnexceptionalPredsOf(e);
				for (int j = 0; j < predes.size(); j++) {
					Unit p = predes.get(j);
					if ((!isInvoke(p)) && (!allUnits.contains(p))) {
						allUnits.add(p);
						expandQueue.add(p);
					}
				}
			}
		}
		return allUnits;
	}

	private void UpdateAllZeros() {
		Set<Unit> allDepthZeros = getAllDepthZeroUnites();
		for (Unit e : allDepthZeros) {
			this.updateLowest(e, 0);
		}
	}

	private void updateLowest(Unit e, int depth) {
		if (this.lowestUnwind.containsKey(e)) {
			int currentDepth = this.lowestUnwind.remove(e);
			if (depth < currentDepth) {
				this.lowestUnwind.put(e, depth);
				this.ifModified = true;
			} else {
				this.lowestUnwind.put(e, currentDepth);
			}
		} else {
			this.lowestUnwind.put(e, depth);
			this.ifModified = true;
		}
	}

	private void updateShortest(Unit e, int length) {
		if (this.shortestPath.containsKey(e)) {
			int currentLength = this.shortestPath.remove(e);
			if (length < currentLength) {
				this.shortestPath.put(e, length);
			} else {
				this.shortestPath.put(e, currentLength);
			}
		} else {
			this.shortestPath.put(e, length);
		}
	}

	private void findReturnUnit(Set<Unit> visited, Queue<Unit> expandQueue) {
		if (expandQueue.isEmpty()) {
			return;
		} else {
			Unit e = expandQueue.remove();
			if ((e instanceof ReturnStmt) || (e instanceof ReturnVoidStmt)) {
				this.returnUnit.add(e);
			} else {
				List<Unit> successors = this.cfg.getUnexceptionalSuccsOf(e);
				for (int i = 0; i < successors.size(); i++) {
					Unit successor = successors.get(i);
					if (!visited.contains(successor)) {
						visited.add(successor);
						expandQueue.add(successor);
					}
				}
			}
			findReturnUnit(visited, expandQueue);
		}
	}

	public boolean IfRootHasDepth() {
		if (this.lowestUnwind.containsKey(this.root)) {
			return true;
		} else {
			return false;
		}
	}

	public int getRootDepth() {
		return this.lowestUnwind.get(this.root);
	}

	public static boolean isInvoke(Unit u) {
		if (u instanceof InvokeStmt) {
			String signature = getMethodSignature(u);
			if (signature.equals("<java.lang.Object: void <init>()>")) {
				return false;
			}
			return true;
		} else {
			if (u instanceof AssignStmt) {
				Value right = ((AssignStmt) u).getRightOp();
				if (right instanceof InvokeExpr) {
					String signature = getMethodSignature(u);
					if (signature.equals("<java.lang.Object: void <init>()>")) {
						return false;
					}
					return true;
				}
			}
		}
		return false;
	}
	public boolean IfNonAnalysis(Unit u){
		if(!isInvoke(u)){
			return true;
		}
		else{
			String signature=getMethodSignature(u);
			if(!this.stores.containsKey(signature)){
				return true;
			}
		}
		return false;
	}
	public static String getMethodSignature(Unit u) {
		if (u instanceof InvokeStmt) {
			InvokeStmt iStmt = (InvokeStmt) u;
			return iStmt.getInvokeExpr().getMethod().getSignature();
		} else {
			if (u instanceof AssignStmt) {
				Value right = ((AssignStmt) u).getRightOp();
				InvokeExpr iExpr = (InvokeExpr) right;
				return iExpr.getMethod().getSignature();
			}
		}
		return null;
	}

	public boolean ifModified() {
		return this.ifModified;
	}

	public int getNodeDepth(Unit u) {
		if(!this.lowestUnwind.containsKey(u)){
			return Integer.MAX_VALUE;
		}
		return this.lowestUnwind.get(u);
	}

	public int getNodeShortest(Unit u) {
		if (!this.shortestPath.containsKey(u)) {
			System.out.println(u);
			return Integer.MAX_VALUE;
		}
		return this.shortestPath.get(u);
	}

	public void printResult() {
		for (Entry<Unit, Integer> e : this.lowestUnwind.entrySet()) {
			System.out.println("Unit:" + e.getKey().toString() + ",cost:"
					+ e.getValue());
		}
	}

	public void printResult2() {
		for (Entry<Unit, Integer> e : this.shortestPath.entrySet()) {
			System.out.println("Unit:" + e.getKey().toString() + ",cost:"
					+ e.getValue());
		}
	}

	public Unit getRoot() {
		return this.root;
	}
}
