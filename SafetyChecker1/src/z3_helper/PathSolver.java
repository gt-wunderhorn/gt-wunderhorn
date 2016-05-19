package z3_helper;

import java.util.ArrayList;
import java.util.Map;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.InterpolationContext.ComputeInterpolantResult;
import com.microsoft.z3.Params;
import com.microsoft.z3.Sort;
import com.microsoft.z3.enumerations.Z3_lbool;

import infoFlow.Forest;
import infoFlow.Node;

public class PathSolver {
	private ArrayList<ArrayList<Node>> paths;
	private Forest forest;
	private InterpolationContext ictx;
	private BoolExpr policyAndAssign;
	private BoolExpr[] InterpolantResult;
	private boolean IfCorrect;
	private ArrayList<PathCoverter> allCoverters;
	private BoolExpr[] testFor;

	public PathSolver(ArrayList<ArrayList<Node>> errorPaths, Forest forest) {
		this.paths = errorPaths;
		this.forest = forest;
		this.ictx = this.forest.getIctx();
		this.IfCorrect = true;
		this.allCoverters = new ArrayList<PathCoverter>();
	}

	public boolean CalSelfCompositionResult(ArrayList<Node> path1) {
		this.allCoverters.clear();
		PathCoverter Coverter1 = new PathCoverter(0, 1, this.ictx, path1, this.forest.getSubFunctionTree(),this.forest);
//		PathCoverter Coverter2 = new PathCoverter(1, 2, this.ictx, path2, this.forest.getSubFunctionTree(),this.forest);
		this.allCoverters.add(Coverter1);
//		this.allCoverters.add(Coverter2);
		PathHelper Helper1 = new PathHelper(Coverter1, this.forest.getSubFunctionTree(), path1,this.forest);
//		PathHelper Helper2 = new PathHelper(Coverter2, this.forest.getSubFunctionTree(), path2,this.forest);
		Helper1.ConvertToZ3();
//		Helper2.ConvertToZ3();
		BoolExpr inter1 = Coverter1.getInterpolant();
//		BoolExpr inter2 = Coverter2.getInterpolant();
//		BoolExpr inter = this.ictx.mkAnd(inter1, inter2);
		System.out.println("*********\n" + inter1 + "\n********\nPathSolver.CalSelfCompositionResult");
//		BoolExpr InterpolantAndPolicy = this.ictx.mkAnd(inter1, this.generateAssignAndPolicy());
		Params params = this.ictx.mkParams();
//		ComputeInterpolantResult result = this.ictx.ComputeInterpolant(InterpolantAndPolicy, params);
		ComputeInterpolantResult result = this.ictx.ComputeInterpolant(inter1, params);
		Z3_lbool status = result.status;
	        boolean r;	
		if (status == Z3_lbool.Z3_L_FALSE) {
			this.InterpolantResult = result.interp;
			/**
			 * System.out.println("interp result"); for(int
			 * i=0;i<result.interp.length;i++){
			 * System.out.println(result.interp[i]); }
			 **/

			r =  false;
		} else {
			r = true;
		}

		System.out.println("r=" + r);
		System.out.println("PathSolver.CalSelfCompositionResult");
		return r;
	}

	public void CalResult() {
		int size = this.paths.size();
		/*
		 * System.out.println("---------------------------"); for(int
		 * i=0;i<size;i++){ System.out.println("the"+i+"th path is");
		 * PathCoverter.printPath(this.paths.get(i)); }
		 */
		System.out.println(size);
		if(size == 2) throw new RuntimeException();
		ArrayList<Node> theNewPath = this.paths.get(size - 1);
		boolean result = this.CalSelfCompositionResult(theNewPath);
		if (!result) {
			this.IfCorrect = true;
			this.refine();
			for (int i = 0; i < (size - 1); i++) {
				ArrayList<Node> comparePath = this.paths.get(i);
				boolean r = this.CalSelfCompositionResult(comparePath);

				if (r) {
					this.refine();
					this.IfCorrect = true;
				} else {
					this.IfCorrect = false;
					break;
				}
			}
		} else {
			this.IfCorrect = false;
		}
	}

	public BoolExpr generateAssignAndPolicy() {
		int outputSize = this.allCoverters.get(0).getErrorCheck().size();
		if(outputSize<1){
			return this.ictx.mkFalse();
		}
		Map<Integer, Expr> outputSample = this.allCoverters.get(0)
				.getErrorCheck();
		Sort[] outputSort = new Sort[outputSample.size()];
		for (int i = 0; i < outputSample.size(); i++) {
			Expr theOne = outputSample.get(i);
			System.out.println("****\n" + theOne + "--" + theOne.getSort());
			outputSort[i] = theOne.getSort();
		}
		System.out.println("PathSolver.generateAssignAndPolicy");
		System.exit(0);

		Expr[] output1 = new Expr[outputSize];
//		Expr[] output2 = new Expr[outputSize];
		BoolExpr[] policyAll = new BoolExpr[outputSize-1];
		for (int i = 0; i < outputSize; i++) {
			Expr output1P1 = this.ictx.mkConst("output1+parameter" + i,
					outputSort[i]);
//			Expr output1P2 = this.ictx.mkConst("output2+parameter" + i,
//					outputSort[i]);
//			policyAll[i] = ictx.mkEq(output1P1, output1P2);
			output1[i] = output1P1;
//			output2[i] = output1P2;
		}
		policyAll[0] = ictx.mkEq(output1[0], output1[1]);
		if(ictx == null)
			System.out.println("ictx is null");
		System.out.println(policyAll);
//		BoolExpr policy = this.ictx.mkNot(this.ictx.mkAnd(policyAll));
		BoolExpr policy = this.ictx.mkAnd(policyAll);
		BoolExpr output1Assign[] = new BoolExpr[outputSize];
//		BoolExpr output2Assign[] = new BoolExpr[outputSize];
		Map<Integer, Expr> output1R = this.allCoverters.get(0).getErrorCheck();
//		Map<Integer, Expr> output2R = this.allCoverters.get(1).getErrorCheck();
		for (int k = 0; k < outputSize; k++) {
			BoolExpr f1 = this.ictx.mkEq(output1[k], output1R.get(k));
//			BoolExpr f2 = this.ictx.mkEq(output2[k], output2R.get(k));
			output1Assign[k] = f1;
//			output2Assign[k] = f2;
		}
		BoolExpr formula=null;
		if (this.allCoverters.get(0).returnSensitive().size() > 0) {
			Expr sensitive1 = this.allCoverters.get(0).returnSensitive().get(0);
//			Expr sensitive2 = this.allCoverters.get(1).returnSensitive().get(0);
			Expr sensitive2 = this.allCoverters.get(0).returnSensitive().get(1);
			BoolExpr sensitiveEqual = this.ictx.mkEq(sensitive1, sensitive2);
			BoolExpr sensitiveNotEqual = this.ictx.mkNot(sensitiveEqual);
			BoolExpr all1Assign = this.ictx.mkAnd(output1Assign);
//			BoolExpr all2Assign = this.ictx.mkAnd(output2Assign);
//			BoolExpr orAssign = this.ictx.mkAnd(all1Assign, all2Assign);
			BoolExpr orAssign = all1Assign;
			 formula = this.ictx.mkAnd(orAssign, policy,
					sensitiveNotEqual);
		} else {
			BoolExpr all1Assign = this.ictx.mkAnd(output1Assign);
//			BoolExpr all2Assign = this.ictx.mkAnd(output2Assign);
//			BoolExpr orAssign = this.ictx.mkAnd(all1Assign, all2Assign);
			BoolExpr orAssign = all1Assign;
			formula = this.ictx.mkAnd(orAssign, policy);
		}
		/**
		 * ArrayExpr array=this.ictx.mkArrayConst("output", ictx.getIntSort(),
		 * ictx.getIntSort()); BoolExpr[] constrains=new BoolExpr[3]; Expr
		 * select1=this.ictx.mkSelect(array, ictx.mkInt(0)); Expr
		 * select2=this.ictx.mkSelect(array, ictx.mkInt(1)); Expr
		 * select3=this.ictx.mkSelect(array, ictx.mkInt(2));
		 * constrains[0]=this.ictx.mkEq(select1, this.ictx.mkInt(96));
		 * constrains[1]=this.ictx.mkEq(select2, this.ictx.mkInt(98));
		 * constrains[2]=this.ictx.mkEq(select3, this.ictx.mkInt(99)); BoolExpr
		 * policy=this.ictx.mkAnd(constrains); Expr two=this.ictx.mkInt(2);
		 * BoolExpr equal=this.ictx.mkEq(array,
		 * this.allCoverters.get(0).getErrorCheck().get(0)); BoolExpr
		 * formula=this.ictx.mkAnd(policy,equal); /** Expr
		 * firstOutput=this.allCoverters.get(0).getErrorCheck().get(0); Expr
		 * SecondOutput=this.allCoverters.get(0).getErrorCheck().get(0);
		 * BoolExpr assign=this.ictx.mkEq(firstOutput, SecondOutput); Expr
		 * sensitive1=this.allCoverters.get(0).returnSensitive().get(0); Expr
		 * sensitive2=this.allCoverters.get(1).returnSensitive().get(0);
		 * BoolExpr sensitiveEqual=this.ictx.mkEq(sensitive1,sensitive2);
		 * BoolExpr sensitiveNotEqual=this.ictx.mkNot(sensitiveEqual); BoolExpr
		 * formula=this.ictx.mkAnd(assign, sensitiveNotEqual);
		 **/
		//System.out.println("the policy is " + formula);
		return formula;
	}

	public void refine() {
		int start = 0;
		for (int i = 0; i < this.allCoverters.size(); i++) {
			PathCoverter theCoverter = this.allCoverters.get(i);
			//System.out.println(this.InterpolantResult.length);
			start = theCoverter.addInvariant(start, this.InterpolantResult);
			System.out.println(start);
		}
	}

	public boolean ifCorrect() {
		return this.IfCorrect;
	}

	public BoolExpr[] getTest() {
		return this.testFor;
	}
}
