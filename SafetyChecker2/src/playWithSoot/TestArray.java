package playWithSoot;

import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import soot.ArrayType;
import soot.Body;
import soot.BodyTransformer;
import soot.Local;
import soot.NullType;
import soot.PackManager;
import soot.RefLikeType;
import soot.RefType;
import soot.SootField;
import soot.SootMethod;
import soot.Transform;
import soot.Type;
import soot.Unit;
import soot.Value;
import soot.jimple.AssignStmt;
import soot.jimple.FieldRef;
import soot.jimple.GotoStmt;
import soot.jimple.IfStmt;
import soot.jimple.InstanceFieldRef;
import soot.jimple.NewArrayExpr;
import soot.options.Options;
import soot.shimple.PhiExpr;
import soot.toolkits.graph.ExceptionalUnitGraph;
import soot.toolkits.scalar.ValueUnitPair;

public class TestArray {
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
				stores.get("<toy_benchmark.ArrayExample: void main(java.lang.String[])>"));
		List<Unit> heads = cfg.getHeads();
		List<Unit> one=cfg.getUnexceptionalSuccsOf(heads.get(0));
		System.out.println(one.get(0));
		AssignStmt A1=(AssignStmt) one.get(0);
		Value a1Left=A1.getLeftOp();
		System.out.println(a1Left.getType());
		List<Unit> two=cfg.getUnexceptionalSuccsOf(A1);
		List<Unit> three=cfg.getUnexceptionalSuccsOf(two.get(0));
		List<Unit> four=cfg.getUnexceptionalSuccsOf(three.get(0));
		System.out.println(four.get(0));
		AssignStmt A2=(AssignStmt) four.get(0);
		Value a2Left=A2.getLeftOp();
		if (a2Left instanceof InstanceFieldRef){
			System.out.println("Field ref");
		}
		InstanceFieldRef fRef=(InstanceFieldRef) a2Left;
		SootField sF=fRef.getField();
		Value base=fRef.getBase();
		System.out.println("base:"+base);
		System.out.println(sF.getDeclaration());
		System.out.println(fRef.getFieldRef());
		System.out.println(a2Left.getType());
		/*
		 * List<Unit> six =cfg.getSuccsOf(five.get(1)); for(Unit element:six){
		 * System.out.println(element.toString()); } Unit error=six.get(0);
		 * if(error instanceof InvokeStmt){ System.out.println(error);
		 * if(error.toString().equals
		 * ("staticinvoke <toy_benchmark.ToyBenchmark1: void Error()>()")){
		 * System.out.println("error works"); }
		 * 
		 * }
		 * 
		 * /* if(four.get(0) instanceof IfStmt){ IfStmt theone=(IfStmt)
		 * four.get(0); Unit next=theone.getTarget(); List<Unit> five
		 * =cfg.getSuccsOf(next); for(Unit element:five){
		 * System.out.println(element.toString()); } List<Unit> six
		 * =cfg.getSuccsOf(five.get(0)); for(Unit element:six){
		 * System.out.println(element.toString()); } List<Unit> seven
		 * =cfg.getSuccsOf(six.get(0)); for(Unit element:seven){
		 * System.out.println(element.toString()); } if(seven.get(0) instanceof
		 * ReturnVoidStmt){ System.out.println("0"); } }
		 */
		/*
		 * List<Unit> five =cfg.getUnexceptionalSuccsOf(four.get(0)); for(Unit
		 * element:five){ System.out.println(element.toString()); } List<Unit>
		 * six =cfg.getUnexceptionalSuccsOf(five.get(0)); for(Unit element:six){
		 * System.out.println(element.toString()); } List<Unit> seven
		 * =cfg.getUnexceptionalSuccsOf(six.get(0)); for(Unit element:seven){
		 * System.out.println(element.toString()); } if(seven.get(0) instanceof
		 * GotoStmt){ GotoStmt theone=(GotoStmt) seven.get(0); Unit
		 * next=theone.getTarget(); System.out.println(next.toString()); }
		 * System.out.println(five.size());
		 */
	}
}