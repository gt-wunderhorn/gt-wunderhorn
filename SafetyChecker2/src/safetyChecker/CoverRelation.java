package safetyChecker;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Solver;
import com.microsoft.z3.Status;

import soot.Unit;

public class CoverRelation {
	private InterpolationContext ictx;
	private Map<Vertex, Set<Vertex>> coveringMap;
	private Map<Vertex, Set<Vertex>> coveredByMap;
	private Set<Vertex> ancestorCoveredSet;
	private Map<Unit, LinkedList<Vertex>> unitVertexMap;

	public CoverRelation(InterpolationContext ictx) {
		this.ictx = ictx;
		coveringMap = new HashMap<Vertex, Set<Vertex>>();
		coveredByMap = new HashMap<Vertex, Set<Vertex>>();
		ancestorCoveredSet = new HashSet<Vertex>();
		unitVertexMap = new HashMap<Unit, LinkedList<Vertex>>(); 

	}

	public void updateUnitVertexMap(Edge edge) {
		Unit unit = edge.getUnit();
		if(this.unitVertexMap.containsKey(unit)) {
			LinkedList<Vertex> vertexList = unitVertexMap.get(edge.getUnit());
			vertexList.add(edge.getSource());	
		} else {
			LinkedList<Vertex> vertexList = new LinkedList<Vertex>();
			vertexList.add(edge.getSource());
			unitVertexMap.put(edge.getUnit(), vertexList);
		}
	}

	public void updateCover() {
	//	clearCovers();		
		cover();
	}

	private void clearCovers() {
		coveringMap.clear();
		coveredByMap.clear();
		ancestorCoveredSet.clear();
	}

	private void cover() {
		LogUtils.debugln(">>>>>>>>>CoverRelation.cover");
		for(Entry<Unit, LinkedList<Vertex>> entry : this.unitVertexMap.entrySet()) {

			LinkedList<Vertex> vertexList = entry.getValue();
			for(int weakerIndex = vertexList.size() - 1; weakerIndex >= 0; weakerIndex--) {
				Vertex weakerVertex = vertexList.get(weakerIndex);
				if(weakerVertex.getInvariant() == null) 
					continue;

				for(int strongerIndex = 0; strongerIndex < weakerIndex; strongerIndex++) {
					Vertex strongerVertex = vertexList.get(strongerIndex);
					if(strongerVertex.getInvariant() == null) 
						continue;

					boolean coveredByResult = checkCoveredBy(weakerVertex, strongerVertex);
					LogUtils.debugln("coveredByResult=" + coveredByResult);
					if(coveredByResult) {
						LogUtils.debugln("----------");
						LogUtils.debugln("weakerVertex=" + weakerVertex + "-" + weakerVertex.getInvariant());
						LogUtils.debugln("strongerVertex=" + strongerVertex + "-" + strongerVertex.getInvariant());
						boolean ancestorCovered = isAncestorCovered(strongerVertex);
						// if one of ancesstors of stronger (covering) vertex is 
						// covered by other vertex. it cannot cover other nodes.
						if(!ancestorCovered)
							addCoverRelation(weakerVertex, strongerVertex);
					}
				}
			}
		}
	}

	private boolean checkCoveredBy(Vertex weakerVertex, Vertex strongerVertex) {
		BoolExpr weakerInvariant = weakerVertex.getInvariant();
		BoolExpr strongerInvariant = strongerVertex.getInvariant();

		return isWeakerThan(weakerInvariant, strongerInvariant);
	}

	private boolean isWeakerThan(BoolExpr weakerInvariant, BoolExpr strongerInvariant) {
		return isStrongerThan(strongerInvariant, weakerInvariant);
	}

	private boolean isStrongerThan(BoolExpr strongerInvariant, BoolExpr weakerInvariant) {

		LogUtils.debugln("weakaer = " + weakerInvariant);
		BoolExpr notWeakerInvariant = this.ictx.mkNot(weakerInvariant);
		LogUtils.debugln("not weaker = " + notWeakerInvariant);
		BoolExpr entailmentExpr = this.ictx.mkAnd(strongerInvariant, notWeakerInvariant);
		LogUtils.debugln("stronger = " + strongerInvariant);

		Solver solver = this.ictx.mkSolver();
		solver.reset();
		solver.add(entailmentExpr);
		Status status = solver.check();
	
		boolean result = false;
		if(status == Status.UNSATISFIABLE) 
			result =  true;
		else 
			result = false;
		LogUtils.debugln("result=" + result);
		return result;
	}

	public boolean isCovered(Vertex vertex) {
		return isDirectlyCovered(vertex) || isAncestorCovered(vertex);
	}

	public boolean isDirectlyCovered(Vertex vertex) {
		Set<Vertex> coveredByList = coveredByMap.get(vertex);
		if(coveredByList == null || coveredByList.size() == 0)
			return false;
		return true;
	}

	public boolean isAncestorCovered(Vertex vertex) {
		return ancestorCoveredSet.contains(vertex);
	}

	private void addCoverRelation(Vertex weakerVertex, Vertex strongerVertex) {
		LogUtils.debugln(">>>>>CoverRelation.addCoverRelation");

		if(coveredByMap.containsKey(weakerVertex)) {
			coveredByMap.get(weakerVertex).add(strongerVertex);
//			if(coveredByMap.get(weakerVertex).size() == 1) 
//				throw new RuntimeException("size must be greater than 1: fix addCoverRelation.");
		} else {
			Set<Vertex> ll = new HashSet<Vertex>();
			ll.add(strongerVertex);
			coveredByMap.put(weakerVertex, ll);
			// since this vertex is now covered by a stronger vertex
			// this weak vertex cannot cover other nodes
			clearCoverRelation(weakerVertex);
			if(!isAncestorCovered(weakerVertex)) 
				coverDescendants(weakerVertex);
		}

		if(coveringMap.containsKey(strongerVertex)) {
			coveringMap.get(strongerVertex).add(weakerVertex);
		} else {
			Set<Vertex> ll = new HashSet<Vertex>();
			ll.add(weakerVertex);
			coveringMap.put(strongerVertex, ll);
		}
		LogUtils.debugln("<<<<<CoverRelation.addCoverRelation");
	}
	
	private void clearCoverRelation(Vertex vertex) {
		if(coveringMap.containsKey(vertex)) {
			Set<Vertex> ll = coveringMap.get(vertex);
			for(Vertex coveredVertex : ll) {
				coveredByMap.get(coveredVertex).remove(vertex);
				if(!isAncestorCovered(coveredVertex))  
					uncoverDescendants(coveredVertex);	
			}
			ll.clear();
		}
	}

	private void uncoverDescendants(Vertex vertex) {
		LogUtils.debugln(">>>>>>>>>CoverRelation.uncoverDescendants = " + vertex);
       		for(Vertex prevVertex : vertex.getPreviousVertexSet()) {
			ancestorCoveredSet.remove(prevVertex);
			uncoverDescendants(prevVertex);
		}
	}
	
	private void coverDescendants(Vertex vertex) {
		LogUtils.debugln(">>>>>>>>CoverRelation.coverDescendants = " + vertex);
		for(Vertex prevVertex : vertex.getPreviousVertexSet()) {
			ancestorCoveredSet.add(prevVertex);
			coverDescendants(prevVertex);
		}
	}

	private void isDone(Vertex v) {

	}
	
	public Map<Vertex, Set<Vertex>> getCoveringMap() { return this.coveringMap; }
	public Map<Vertex, Set<Vertex>> getCoveredByMap() { return this.coveredByMap; }
	public Map<Unit, LinkedList<Vertex>> getUnitVertexMap() { return this.unitVertexMap; } 
}
