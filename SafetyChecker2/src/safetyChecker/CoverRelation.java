package safetyChecker;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Solver;
import com.microsoft.z3.Status;

import safetyChecker.utilities.LogUtils;

import soot.Unit;

public class CoverRelation {
	private InterpolationContext ictx;
	private Map<Vertex, Set<Vertex>> coveringMap;
	private Map<Vertex, Vertex> coveredByMap;
	private Set<Vertex> ancestorCoveredSet;
	private Map<Unit, LinkedList<Vertex>> unitVertexMap;
	private ProgramTree programTree;

	public CoverRelation(InterpolationContext ictx, ProgramTree programTree) {
		this.ictx = ictx;
		this.programTree = programTree;
		coveringMap = new HashMap<Vertex, Set<Vertex>>();
		coveredByMap = new HashMap<Vertex, Vertex>();
		ancestorCoveredSet = new HashSet<Vertex>();
		unitVertexMap = new HashMap<Unit, LinkedList<Vertex>>(); 

	}

	public void updateUnitVertexMap(Vertex vertex) {
		Edge edge = vertex.getOutgoingEdge();
		Unit unit = edge.getUnit();
		LogUtils.debugln("add " + vertex + "--" + unit);
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
		cover();
	}

	private void cover() {
		LogUtils.debugln(">>>>>>>>>CoverRelation.cover");
		for(Entry<Unit, LinkedList<Vertex>> entry : this.unitVertexMap.entrySet()) {

			LinkedList<Vertex> vertexList = entry.getValue();
			for(int weakerIndex = vertexList.size() - 1; weakerIndex >= 0; weakerIndex--) {
				LogUtils.debugln("weakerVeertex=" + vertexList.get(weakerIndex) + "--" + vertexList.get(weakerIndex).getOutgoingEdge());
				LogUtils.debugln("weakerControl=" + this.findPrevControlLocation(vertexList.get(weakerIndex)));
				Vertex weakerVertex = vertexList.get(weakerIndex);
				if(weakerVertex == null || weakerVertex.getInvariant() == null) 
					continue;

				if(this.isCovered(weakerVertex))
					continue;

				for(int strongerIndex = 0; strongerIndex < weakerIndex; strongerIndex++) {
					Vertex strongerVertex = vertexList.get(strongerIndex);

					if(this.isCovered(weakerVertex))
						continue;

					if(this.isCovered(strongerVertex))
							continue;

					if(strongerVertex.getInvariant() == null) 
						continue;

					boolean coveredByResult = checkCoveredBy(weakerVertex, strongerVertex);
					LogUtils.debugln("coveredByResult=" + coveredByResult);
					if(coveredByResult) {
						LogUtils.debugln("----------");
						LogUtils.debugln("weakerVertex=" + weakerVertex + "-" + weakerVertex.getInvariant());
						LogUtils.debugln("strongerVertex=" + strongerVertex + "-" + strongerVertex.getInvariant());
						boolean covered = this.isCovered(strongerVertex);
						LogUtils.debugln("***" + strongerVertex + "--" + covered);
						// if one of ancesstors of stronger (covering) vertex is 
						// covered by other vertex. it cannot cover other nodes.
						if(!covered)
							addCoverRelation(weakerVertex, strongerVertex);
					}
				}
			}
		}
	}
	 
	private Vertex findPrevControlLocation(Vertex vertex) {
		for(Edge incoming : vertex.getIncomingEdges()) {
			if(incoming.isControlLocation())
				return incoming.getSource();
		}
		return null;
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
		LogUtils.debugln("--->coverRelation.isStrongerThan");

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
		solver.dispose();
		LogUtils.debugln("<---coverRelation.isStrongerThan");
		return result;
	}

	public boolean isCovered(Vertex vertex) {
		return isDirectlyCovered(vertex) || isAncestorCovered(vertex);
	}

	public boolean isDirectlyCovered(Vertex vertex) {
		if(coveredByMap.containsKey(vertex))
			return true;
		return false;
	}

	public boolean isAncestorCovered(Vertex vertex) {
		return ancestorCoveredSet.contains(vertex);
	}

	int counter =0;
	private void addCoverRelation(Vertex weakerVertex, Vertex strongerVertex) {
		LogUtils.debugln(">>>>>CoverRelation.addCoverRelation");
		LogUtils.debugln("weaker= " + weakerVertex + "--stronger=" + strongerVertex);

		if(this.coveredByMap.containsKey(weakerVertex)) {
			// since it is covered once no need to add another cover
			return;
		} else {
			LogUtils.debugln("else coveredByMap. not contains");
			this.coveredByMap.put(weakerVertex, strongerVertex);
			// since this vertex is now covered by a stronger vertex
			// this weak vertex cannot cover other nodes
			if(!this.isAncestorCovered(weakerVertex)) 
				this.coverDescendants(weakerVertex);
			this.clearCoverRelation(weakerVertex);
		}

		if(this.coveringMap.containsKey(strongerVertex)) {
			this.coveringMap.get(strongerVertex).add(weakerVertex);
		} else {
			Set<Vertex> ll = new HashSet<Vertex>();
			ll.add(weakerVertex);
			this.coveringMap.put(strongerVertex, ll);
		}
		LogUtils.debugln("<<<<<CoverRelation.addCoverRelation");
	}
	
	protected void checkHoldsAndClearCoverRelation(Vertex vertex) {
		if(this.coveringMap.containsKey(vertex)) {
			LinkedList<Vertex> toBeRemoved = new LinkedList<Vertex>();
			Set<Vertex> ll = this.coveringMap.get(vertex); 
			for(Vertex coveredVertex : ll) {
				// check if stil holds
				// if holds do not clear, otherwise clear
				boolean stillHolds = this.isWeakerThan(coveredVertex.getInvariant(), vertex.getInvariant());
				if(!stillHolds) {
					LogUtils.debugln("^^^^^^checkHoldsAndClearCoverRelation=" + vertex + "--" + coveredVertex);
					this.add2UncoveredMap(coveredVertex);
					toBeRemoved.add(coveredVertex);
					this.coveredByMap.remove(coveredVertex);
					if(!this.isAncestorCovered(coveredVertex))
						this.uncoverDescendants(coveredVertex);
				}

			}
		}
	}

	private void clearCoverRelation(Vertex vertex) {
		LogUtils.debugln("*******clearCoverRelation=" + vertex);
		if(coveringMap.containsKey(vertex)) {
			Set<Vertex> ll = coveringMap.get(vertex);
			for(Vertex coveredVertex : ll) {
				this.add2UncoveredMap(coveredVertex);
				coveredByMap.remove(coveredVertex);
				if(!isAncestorCovered(coveredVertex))  
					uncoverDescendants(coveredVertex);	
			}
			ll.clear();
		}
	}

	private void uncoverDescendants(Vertex vertex) {
		LogUtils.debugln(">>>>>>>>>CoverRelation.uncoverDescendants = " + vertex);
       		for(Vertex prevVertex : vertex.getPreviousVertexSet()) {
			this.add2UncoveredMap(prevVertex);
			ancestorCoveredSet.remove(prevVertex);
			uncoverDescendants(prevVertex);
		}
	}

	private void add2UncoveredMap(Vertex vertex) {
		if(vertex.getPreviousVertexSet().size() < this.programTree.getCfg().getUnexceptionalPredsOf(vertex.getOutgoingEdge().getUnit()).size())
			this.programTree.getUncovered().add(vertex);

	}
	
	private void coverDescendants(Vertex vertex) {
		LogUtils.debugln(">>>>>>>>CoverRelation.coverDescendants = " + vertex);
		for(Vertex prevVertex : vertex.getPreviousVertexSet()) {
			ancestorCoveredSet.add(prevVertex);
			coverDescendants(prevVertex);
		}
	}

	public Map<Vertex, Set<Vertex>> getCoveringMap() { return this.coveringMap; }
	public Map<Vertex, Vertex> getCoveredByMap() { return this.coveredByMap; }
	public Map<Unit, LinkedList<Vertex>> getUnitVertexMap() { return this.unitVertexMap; } 
}
