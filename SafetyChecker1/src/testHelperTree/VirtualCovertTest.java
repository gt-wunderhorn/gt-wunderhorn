package testHelperTree;

import infoFlow.Forest;
import infoFlow.Node;
import infoFlow.Tree;

import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import soot.Body;
import soot.BodyTransformer;
import soot.PackManager;
import soot.SootMethod;
import soot.Transform;
import soot.options.Options;

public class VirtualCovertTest {
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
		Forest forest1=new Forest(stores,"<toy_benchmark.RecrusionF1: void f5(int)>");
		Tree theTree=forest1.getTree("<toy_benchmark.RecrusionF1: void f5(int)>");
		theTree.getNewReturnPath();
		ArrayList<Node> thePath=theTree.getPath(0);
		for(int i=0;i<thePath.size();i++){
			System.out.println(thePath.get(i).getStmt());
		}
		Set<Node> leafs=theTree.getLeaf();
		System.out.println("Leaf");
		for(Node n:leafs){
			System.out.println(n.getStmt());
		}
		theTree.getNewReturnPath();
		ArrayList<Node> thePath2=theTree.getPath(1);
		for(int i=0;i<thePath2.size();i++){
			System.out.println(thePath2.get(i).getStmt());
		}
	}
}
