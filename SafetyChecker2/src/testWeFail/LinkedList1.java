package testWeFail;

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

public class LinkedList1 {
	static Map<String, Body> stores = new HashMap<String, Body>();

	public static void main(String[] args) throws FileNotFoundException,
			UnsupportedEncodingException {
		Options.v().set_src_prec(Options.src_prec_c);
		Options.v().set_output_format(Options.output_format_shimple);
		Options.v().set_allow_phantom_refs(true);
		String[] sootArgs = new String[] { "-process-dir",
				"C:/Users/qizhou/workspace/InfoFlow1/BechMark/WeFail/FailLinkedList/bin", "-output-dir",
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
				"<LinkedList1: void test(int)>");
		forest1.TestCorrect();
		Map<String,Tree> theTree=forest1.getSubFunctionTree();
		for(Entry<String,Tree> e : theTree.entrySet()){
			Tree tree=e.getValue();
			if (tree.sizeOfPath()>0){
			ArrayList<Node> path=tree.getPath(0);
			System.out.println(e.getKey()+"path");
			for(int i=0;i<path.size();i++){
				System.out.println(path.get(i).getStmt());
				System.out.println(path.get(i).getInvariant());
			}
			}
		}
		System.out.println(forest1.ifCorrect());
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
		System.out.println(forest1.ifCorrect());
	}
}
