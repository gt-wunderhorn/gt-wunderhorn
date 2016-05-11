package z3_helper;

import infoFlow.HelpTree;
import infoFlow.Node;
import infoFlow.Tree;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import soot.SootMethod;
import soot.Unit;
import soot.Value;
import soot.jimple.AssignStmt;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;

public class FunctionHelper {
/**	static Map<String, Integer> pathControl = new HashMap<String, Integer>();

	private static String getKey(ArrayList<Integer> path) {
		String result = "";
		for (int i = 0; i < path.size(); i++) {
			int theNumber = path.get(i);
			result = result + theNumber + "#";
		}
		return result;
	}

	public static ArrayList<ArrayList<Integer>> virtualConvert(
			ArrayList<Node> path, Map<String, Tree> functions,
			ArrayList<Integer> thisPath, Stack<Tree> latestExtensionTree) {
		ArrayList<ArrayList<Integer>> result = new ArrayList<ArrayList<Integer>>();
		result.add(thisPath);
		for (int i = 0; i < path.size(); i++) {
			ArrayList<ArrayList<Integer>> newResult = new ArrayList<ArrayList<Integer>>();
			Node n=path.get(i);
			Unit u = n.getStmt();
			if ((HelpTree.isInvoke(u)&&(!n.ifDummy())&&(!n.ifError()))) {
				Tree theFunction = functions
						.get(HelpTree.getMethodSignature(u));
				newResult = oneInvoke(result, functions, theFunction,
						latestExtensionTree);
				result=newResult;
			}
			int c=0;
		}
		return result;
	}

	private static ArrayList<ArrayList<Integer>> oneInvoke(
			ArrayList<ArrayList<Integer>> result, Map<String, Tree> functions,
			Tree theFunction, Stack<Tree> latestExtensionTree) {
		ArrayList<ArrayList<Integer>> newResult = new ArrayList<ArrayList<Integer>>();
		for (int j = 0; j < result.size(); j++) {
			ArrayList<Integer> theArray = result.get(j);
			String key = getKey(theArray);
			if (theFunction.sizeOfPath() == 0) {
				theFunction.getNewReturnPath();
				latestExtensionTree.push(theFunction);
			}
			int pathSelect = 1;
			// we want to check if this position is the first time we met, if it
			// is, we only give one path.
			// if not, we give one more path
			if (pathControl.containsKey(key)) {
				int olderSize = pathControl.get(key);
					olderSize++;
				pathSelect = olderSize;
				pathControl.remove(key);
				pathControl.put(key, olderSize);
			} else {
				pathControl.put(key, pathSelect);
			}
			if (pathSelect < theFunction.sizeOfPath()) {
				theFunction.setAllPathUsed(false);
			} else {
				pathSelect = theFunction.sizeOfPath();
			}
			for (int k = 0; k < pathSelect; k++) {
				ArrayList<Integer> newArray = new ArrayList<Integer>();
				newArray.addAll(theArray);
				newArray.add(k);
				ArrayList<Node> nextPath = theFunction.getPath(k);
				ArrayList<ArrayList<Integer>> RecrusionResult = virtualConvert(
						nextPath, functions, newArray, latestExtensionTree);
				for (int l = 0; l < RecrusionResult.size(); l++) {
					ArrayList<Integer> onePossible = RecrusionResult.get(l);
					newResult.add(onePossible);
				}
			}
		}
		return newResult; 
	} **/
}
