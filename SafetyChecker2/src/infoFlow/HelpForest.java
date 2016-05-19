package infoFlow;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import soot.Body;
import soot.toolkits.graph.ExceptionalUnitGraph;

public class HelpForest {
	private Map<String, HelpTree> functionTrees;

	public HelpForest(Map<String, Body> stores) {
		this.functionTrees = new HashMap<String, HelpTree>();
		for (Entry<String, Body> e : stores.entrySet()) {
			ExceptionalUnitGraph cfg = new ExceptionalUnitGraph(e.getValue());
			HelpTree theTree = new HelpTree(cfg);
			this.functionTrees.put(e.getKey(), theTree);
		}
		for (Entry<String, HelpTree> e : this.functionTrees.entrySet()) {
			e.getValue().setStores(functionTrees);
		}
	}

	public void calculateDepth() {
		boolean NotModified = false;
		while (!NotModified) {
			NotModified=true;
			for (Entry<String, HelpTree> e : this.functionTrees.entrySet()) {
				e.getValue().getDepth();
				if (e.getValue().ifModified()){
					NotModified=false;
				}
			}
		}
	}
	public boolean ifEnd(){
		boolean result=true;
		for (Entry<String, HelpTree> e : this.functionTrees.entrySet()) {
			if (!e.getValue().IfRootHasDepth()){
				result=false;
			}
		}
		return result;
	}
	public Map<String,HelpTree> getResult(){
		return this.functionTrees;
	}
}
