package testDrive;

import infoFlow.Tree;

import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;

import soot.Body;
import soot.BodyTransformer;
import soot.PackManager;
import soot.SootMethod;
import soot.Transform;
import soot.options.Options;
import soot.toolkits.graph.ExceptionalUnitGraph;

public class TestObject {
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
		System.out.println(stores.size());
		ExceptionalUnitGraph cfg = new ExceptionalUnitGraph(
				stores.get("<toy_benchmark.ToyBenchmark3: void main(java.lang.String[])>"));
		Tree theTree=new Tree(cfg);
		if (theTree.ifCorrect()){
			System.out.println("The program is correct");
		}
		else{
			System.out.println("The Program has a bug");
		}
		/** Node root=theTree.getRoot();
		Stack<Node> node=new Stack<Node>();
		node.push(root);
		ArrayList<ArrayList<Node>> twoErrorPath=new ArrayList<ArrayList<Node>>();
		int count=0;
		while((!node.isEmpty())&& (count<5)){
			Node current=node.pop();
			current.expand();
			Set<Node> successors=current.successors();
			for(Node successor:successors){
				if(successor.ifError()){
				count++;
				ArrayList<Node> path=successor.path();
				twoErrorPath.add(path);
				System.out.println(count);
				}
				else{
				node.push(successor);	
				}
			}
		}
		PathSolver p=new PathSolver(theTree.getIctx());
		p.addPath(twoErrorPath.get(0));
		Expr b0=theTree.getIctx().mkIntConst("dsfadfadsfasdfsadfdsaf");
		Expr one=theTree.getIctx().mkInt(0);
		BoolExpr f1=theTree.getIctx().mkEq(b0, one);
		BoolExpr notF1=theTree.getIctx().mkNot((BoolExpr)f1);
		p.refine();
		for(int i=0;i<twoErrorPath.get(0).size();i++){
			System.out.println(twoErrorPath.get(0).get(i).getInvariant());
		}
		**/
	}
}
