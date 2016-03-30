package playWithSoot;

import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import soot.Body;
import soot.BodyTransformer;
import soot.Local;
import soot.PackManager;
import soot.PrimType;
import soot.RefType;
import soot.SootMethod;
import soot.Transform;
import soot.Type;
import soot.Unit;
import soot.Value;
import soot.baf.WordType;
import soot.jimple.AssignStmt;
import soot.jimple.GotoStmt;
import soot.jimple.IfStmt;
import soot.jimple.StringConstant;
import soot.jimple.internal.JimpleLocal;
import soot.options.Options;
import soot.toolkits.graph.ExceptionalUnitGraph;

public class testString {
	static Map<String,Body> stores=new HashMap<String,Body>();
	public static void main(String[] args) throws FileNotFoundException, UnsupportedEncodingException {
		Options.v().set_src_prec(Options.src_prec_c);
		Options.v().set_output_format(Options.output_format_shimple);
		Options.v().set_allow_phantom_refs(true);
		String[] sootArgs=new String[]{
			"-process-dir","C:/Users/qizhou/workspace/toy_benchmark/bin",
			"-output-dir","src/output"
		};
		PackManager.v().getPack("stp").add(new Transform("stp.test", 
				new BodyTransformer() {
					
					@Override
					protected void internalTransform(Body body, String phaseName,
							Map<String, String> options) {
						//hack here
						SootMethod method = body.getMethod();
						String methodSig = method.getSignature();
						System.out.println(methodSig);
						/* System.out.println(method.getName()); */
						stores.put(methodSig, body);
						
				  
					}
				}
				));
		soot.Main.main(sootArgs); 
		System.out.println(stores.size());
		ExceptionalUnitGraph cfg=new ExceptionalUnitGraph(stores.get("<toy_benchmark.StringExample: void main(java.lang.String[])>"));
		List<Unit> heads=cfg.getHeads();
		List<Unit> one=cfg.getUnexceptionalSuccsOf(heads.get(0));
		Unit string=one.get(0);
		AssignStmt aStmt=(AssignStmt)string;
		Value left=aStmt.getLeftOp();
		Value right=aStmt.getRightOp();
		Type leftType=left.getType();
		System.out.println(leftType.getClass());
		if(leftType instanceof RefType){
			System.out.println("left");
		}
		System.out.println(right.getClass());
		System.out.println(leftType);
		StringConstant a=(StringConstant) right;
		String c=a.value;
		
		
	}
	
}
