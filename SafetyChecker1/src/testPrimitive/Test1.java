package testPrimitive;

import infoFlow.Forest;
import infoFlow.HelpForest;
import infoFlow.HelpTree;
import infoFlow.Node;
import infoFlow.Tree;

import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.microsoft.z3.BoolExpr;

import soot.Body;
import soot.BodyTransformer;
import soot.PackManager;
import soot.SootMethod;
import soot.Transform;
import soot.options.Options;
import z3_helper.PathCoverter;
import z3_helper.PathHelper;
import z3_helper.PathSolver;

public class Test1 {
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
				"<toy_benchmark.FunctionP1: int test()>");
		Tree theTree=forest1.getTree("<toy_benchmark.FunctionP1: int test()>");
		ArrayList<Node> errorPath=theTree.getAllErrorPath().get(0);
		for(int i=0;i<errorPath.size();i++){
			Node n=errorPath.get(i);
			System.out.println(n.getStmt());
		}
		PathSolver pSolver=new PathSolver(theTree.getAllErrorPath(),forest1);
	}
}
