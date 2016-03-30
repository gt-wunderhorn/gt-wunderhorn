package testArray;

import infoFlow.Forest;
import infoFlow.Node;
import infoFlow.Tree;

import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import soot.Body;
import soot.BodyTransformer;
import soot.PackManager;
import soot.SootMethod;
import soot.Transform;
import soot.options.Options;

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
				"<toy_benchmark.ToyBenchmark6: void test1(int)>");
		forest1.TestCorrect();
		Map<String, Tree> theTree = forest1.getSubFunctionTree();
		for (Entry<String, Tree> e : theTree.entrySet()) {
			Tree tree = e.getValue();
			if (tree.sizeOfPath() > 0) {
				for (int j = 0; j < tree.sizeOfPath(); j++) {
					ArrayList<Node> path = tree.getPath(j);
					System.out.println("callee path" + j);
					for (int i = 0; i < path.size(); i++) {
						System.out.println(path.get(i).getStmt());
						System.out.println(path.get(i).getInvariant());
					}

				}
			}
		}
		Tree mainTree = forest1.getMainTree();
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
		if (forest1.ifCorrect()) {
			System.out.println("this program is correct");
		} else {
			System.out.println("this program has a bug");
		}
	}
}
