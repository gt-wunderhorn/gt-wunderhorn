package testObject;

import infoFlow.Forest;
import infoFlow.Node;
import infoFlow.Tree;

import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.Params;
import com.microsoft.z3.InterpolationContext.ComputeInterpolantResult;
import com.microsoft.z3.enumerations.Z3_lbool;

import soot.Body;
import soot.BodyTransformer;
import soot.PackManager;
import soot.SootMethod;
import soot.Transform;
import soot.options.Options;
import z3_helper.PathCoverter;
import z3_helper.PathSolver;

public class TestObjectAndField {
	static Map<String, Body> stores = new HashMap<String, Body>();

	public static void main(String[] args) throws FileNotFoundException,
			UnsupportedEncodingException {
		Options.v().set_src_prec(Options.src_prec_c);
		Options.v().set_output_format(Options.output_format_shimple);
		Options.v().set_allow_phantom_refs(true);
		String[] sootArgs = new String[] { "-process-dir",
				"C:/Users/qizhou/workspace/toy_benchmark/bin", "-output-dir",
				"src/output" };
		PackManager.v().getPack("stp")
				.add(new Transform("stp.test", new BodyTransformer() {

					@Override
					protected void internalTransform(Body body,
							String phaseName, Map<String, String> options) {
						// hack here
						SootMethod method = body.getMethod();
						String methodSig = method.getSignature();
						System.out.println(methodSig);
						/* System.out.println(method.getName()); */
						stores.put(methodSig, body);

					}
				}));
		soot.Main.main(sootArgs);
		Forest forest1 = new Forest(stores,
				"<toy_benchmark.ToyBenchmark2: void test1(int)>");
		forest1.TestCorrect();
	
		Map<String, Tree> theTree = forest1.getSubFunctionTree();
		for (Entry<String, Tree> e : theTree.entrySet()) {
			Tree tree = e.getValue();
			if (tree.sizeOfPath() > 0) {
				for (int j = 0; j < tree.sizeOfPath(); j++) {
					ArrayList<Node> path = tree.getPath(j);
					System.out.println(e.getValue().getSignature());
					System.out.println("very special path"+j);
					/**for (int i = 0; i < path.size(); i++) {
						System.out.println(path.get(i).getStmt());
						System.out.println(path.get(i).getInvariant());
						System.out.println("if update:"+path.get(i).getIfUpdate()+"id:"+path.get(i).getId());
						System.out.println("if involve generate interpolant"+path.get(i).ifGenerateInterpolant());
					}**/

				}
			}
		}
		Tree mainTree = forest1.getMainTree();
		System.out.println(forest1.ifCorrect());
		/**ArrayList<ArrayList<Node>> errorPaths = mainTree.getAllErrorPath();
		System.out.println("total size of error paths"+errorPaths.size());
		for(int i=0;i<5;i++){
			ArrayList<Node> errorPath=errorPaths.get(i);
			PathCoverter.printPath(errorPath);
		}
		ArrayList<Node> path1=errorPaths.get(0);
		ArrayList<Node> path2=errorPaths.get(8);
		PathSolver Ps=new PathSolver(errorPaths,forest1);
		boolean result=Ps.CalPairResult(path1, path2);
		System.out.println(result);
	/**	Tree mainTree = forest1.getMainTree(); **/
		ArrayList<ArrayList<Node>> errorPaths = mainTree.getAllErrorPath();
		for (int i = 0; i < errorPaths.size(); i++) {
			System.out.println("path" + i);
			ArrayList<Node> oneErrorPath = errorPaths.get(i);
			for (int j = 0; j < oneErrorPath.size(); j++) {
				System.out.println(oneErrorPath.get(j).getStmt());
				System.out.println(oneErrorPath.get(j).getInvariant()
						.simplify());
			}
		} 
		if(forest1.ifCorrect()){
			System.out.println("this program is correct");
		}
		else{
			System.out.println("this program has a bug");
		}
		//System.out.println(errorPaths.size()+"the size is");
		/**BoolExpr[] test1=forest1.getTest();
		BoolExpr all=forest1.getIctx().mkAnd(test1);
		BoolExpr f1=test1[0];
		BoolExpr f2=test1[4];
		Expr a1=forest1.getIctx().mkIntConst("b2_2Path0Copy0Level0index3");
		Expr a2=forest1.getIctx().mkIntConst("b2_2Path0Copy5Level0index6");
		BoolExpr f3=forest1.getIctx().mkEq(a1, a2);
		BoolExpr f5=forest1.getIctx().mkNot(f3);
		IntExpr a=forest1.getIctx().mkInt("1");
		BoolExpr tribal=forest1.getIctx().mkEq(a, a);
		BoolExpr f4=forest1.getIctx().mkAnd(f1,f2);
		BoolExpr andSet=forest1.getIctx().mkAnd(f4, tribal);
		Params params = forest1.getIctx().mkParams();
		ComputeInterpolantResult result = forest1.getIctx().ComputeInterpolant(
				andSet, params);
		Z3_lbool status = result.status;
		BoolExpr[] result1 = result.interp;
		for(int i=0;i<result1.length;i++){
			System.out.println(result1[i]);
		}	**/
	}
}
