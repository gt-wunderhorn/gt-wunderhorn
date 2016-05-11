package playWithSoot;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import soot.Body;
import soot.BodyTransformer;
import soot.Local;
import soot.Modifier;
import soot.PackManager;
import soot.Scene;
import soot.SootClass;
import soot.SootMethod;
import soot.Transform;
import soot.Unit;
import soot.Value;
import soot.jimple.AddExpr;
import soot.jimple.AssignStmt;
import soot.jimple.BinopExpr;
import soot.jimple.GotoStmt;
import soot.jimple.IfStmt;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;
import soot.jimple.JimpleBody;
import soot.jimple.ReturnStmt;
import soot.jimple.ReturnVoidStmt;
import soot.jimple.internal.JimpleLocal;
import soot.options.Options;
import soot.shimple.PhiExpr;
import soot.tagkit.Tag;
import soot.toolkits.graph.ExceptionalUnitGraph;
import soot.toolkits.scalar.ValueUnitPair;

public class Test2 {
	static Map<String,Body> stores=new HashMap<String,Body>();
	public static void main(String[] args) throws FileNotFoundException, UnsupportedEncodingException {
		Options.v().set_src_prec(Options.src_prec_c);
		Options.v().set_output_format(Options.output_format_shimple);
		Options.v().set_allow_phantom_refs(true);
		String[] sootArgs=new String[]{
			"-process-dir","C:/Users/qizhou/workspace/InfoFlow1/BechMark/bin/classes",
			"-output-dir","src/output/ArraysAndLists_ArrayAccess1"
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
		ExceptionalUnitGraph cfg=new ExceptionalUnitGraph(stores.get("<toy_benchmark.ToyBenchmark1: void main(java.lang.String[])>"));
		List<Unit> heads =cfg.getHeads();
		for(Unit element:heads){
				System.out.println(element.toString());
				if(element instanceof GotoStmt){
					System.out.println(" Goto Statement");
				}
				if(element instanceof IfStmt){
					System.out.println(" If Statement");
					
				}
				if(element instanceof AssignStmt){
					System.out.println(" assign Statement");
				}
		}
		List<Unit> one =cfg.getSuccsOf(heads.get(0));
		for(Unit element:one){
			System.out.println(element.toString());
		}
		
		List<Unit> two =cfg.getSuccsOf(one.get(0));
		for(Unit element:two){
			System.out.println(element.toString());
		}
		List<Unit> three =cfg.getSuccsOf(two.get(0));
		for(Unit element:three){
			System.out.println(element.toString());
			System.out.println("warning");
		}
		Unit theUnit=three.get(0);
		AssignStmt aStmt=(AssignStmt) theUnit;
		Value left=aStmt.getLeftOp();
		Local createLocal=new JimpleLocal("this is the name",left.getType());
		if (createLocal instanceof Local){
			System.out.println("create local is local");
			System.out.println(createLocal);
			System.out.println(createLocal.getType());
			System.out.println(left.getType());
			System.out.println(left.getClass());
		}
		/*
		List<Unit> six =cfg.getSuccsOf(five.get(1));
		for(Unit element:six){
			System.out.println(element.toString());
		}
		Unit error=six.get(0);
		if(error instanceof InvokeStmt){
			System.out.println(error);
			if(error.toString().equals("staticinvoke <toy_benchmark.ToyBenchmark1: void Error()>()")){
				System.out.println("error works");
			}
			
		}
		
		/* if(four.get(0) instanceof IfStmt){
			IfStmt theone=(IfStmt) four.get(0);
			Unit next=theone.getTarget();
			List<Unit> five =cfg.getSuccsOf(next);
			for(Unit element:five){
				System.out.println(element.toString());
			}
			List<Unit> six =cfg.getSuccsOf(five.get(0));
			for(Unit element:six){
				System.out.println(element.toString());
			}
			List<Unit> seven =cfg.getSuccsOf(six.get(0));
			for(Unit element:seven){
				System.out.println(element.toString());
			}
			if(seven.get(0) instanceof   ReturnVoidStmt){
				System.out.println("0");
			} 
		} */
		/* List<Unit> five =cfg.getUnexceptionalSuccsOf(four.get(0));
		for(Unit element:five){
			System.out.println(element.toString());
		}
		List<Unit> six =cfg.getUnexceptionalSuccsOf(five.get(0));
		for(Unit element:six){
			System.out.println(element.toString());
		}
		List<Unit> seven =cfg.getUnexceptionalSuccsOf(six.get(0));
		for(Unit element:seven){
			System.out.println(element.toString());
		}
		if(seven.get(0) instanceof  GotoStmt){
			GotoStmt theone=(GotoStmt) seven.get(0);
			Unit next=theone.getTarget();
			System.out.println(next.toString());
		}
		System.out.println(five.size()); */
	}
	
}
