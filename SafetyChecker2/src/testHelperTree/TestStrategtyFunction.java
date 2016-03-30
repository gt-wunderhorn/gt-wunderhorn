package testHelperTree;

import infoFlow.Forest;
import infoFlow.Node;
import infoFlow.Tree;

import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import soot.Body;
import soot.BodyTransformer;
import soot.PackManager;
import soot.SootMethod;
import soot.Transform;
import soot.options.Options;
import z3_helper.PathSolver;

public class TestStrategtyFunction {
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
		Forest forest1=new Forest(stores,"<toy_benchmark.RecrusionF2: int f1()>");
		Tree mainTree=forest1.getSubFunctionTree().get("<toy_benchmark.RecrusionF2: int f1()>");
		forest1.TestCorrect();
	/**	Tree theTree=forest1.getTree("<toy_benchmark.RecrusionF2: int f1()>");
		Tree theTree2=forest1.getTree("<toy_benchmark.RecrusionF2: int f2(int)>");
		theTree.getNewReturnPath();
		ArrayList<Node> thePath=theTree.getPath(0);
		for(int i=0;i<thePath.size();i++){
			System.out.println(thePath.get(i).getStmt());
		}
		theTree.getNewReturnPath();
		theTree.getNewReturnPath();
		ArrayList<ArrayList<Node>> errorPaths=new ArrayList<ArrayList<Node>>();
		errorPaths.add(thePath);
		errorPaths.add(theTree.getPath(1));
		errorPaths.add(theTree.getPath(2));
		PathSolver pSolver=new PathSolver(errorPaths,forest1);
		pSolver.getAllGuide();
		ArrayList<ArrayList<Integer>> guides=pSolver.getGuideResult();
		for(int i=0;i<guides.size();i++){
			ArrayList<Integer> oneGuide=guides.get(i);
			System.out.println(oneGuide.toString());
		}
		theTree2.getNewReturnPath();
		pSolver.getAllGuide();
		guides=pSolver.getGuideResult();
		for(int i=0;i<guides.size();i++){
			ArrayList<Integer> oneGuide=guides.get(i);
			System.out.println(oneGuide.toString());
		} **/
	}
}
