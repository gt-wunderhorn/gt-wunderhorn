package playWithSoot;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import soot.Body;
import soot.BodyTransformer;
import soot.Modifier;
import soot.PackManager;
import soot.Scene;
import soot.SootClass;
import soot.SootMethod;
import soot.Transform;
import soot.Unit;
import soot.jimple.AssignStmt;
import soot.jimple.GotoStmt;
import soot.jimple.IfStmt;
import soot.jimple.JimpleBody;
import soot.jimple.ReturnStmt;
import soot.jimple.ReturnVoidStmt;
import soot.options.Options;
import soot.toolkits.graph.ExceptionalUnitGraph;
import soot.util.Chain;

public class Test1 {
	static Map<String,Body> stores=new HashMap<String,Body>();
	public static void main(String[] args) throws FileNotFoundException, UnsupportedEncodingException {
		Options.v().set_src_prec(Options.src_prec_c);
		Options.v().set_output_format(Options.output_format_shimple);
		Options.v().set_allow_phantom_refs(true);
		String[] sootArgs=new String[]{
			"-process-dir","C:/Users/qizhou/workspace/InfoFlow1/BechMark/DynamicPatch/InheritedObjects1/bin/classes",
			"-output-dir","src/output/dyanmic"
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
						/*System.out.println(method.getName()); */
						stores.put(method.getName(), body);
						
				  
					}
				}
				));
		
		soot.Main.main(sootArgs);
		Scene a=soot.Scene.v();
		Chain<SootClass> c=a.getClasses();
		for(SootClass sc:c){
			String className=sc.getName();
			if(className.contains("de.ecspride")){
			System.out.println("class name"+sc.getName());
			List<SootMethod> theList=sc.getMethods();
			Chain<SootClass> interfaces=sc.getInterfaces();
			
			}
		}
		System.out.println(stores.size());
		ExceptionalUnitGraph cfg=new ExceptionalUnitGraph(stores.get("main"));
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
		}
		List<Unit> four =cfg.getSuccsOf(three.get(0));
		for(Unit element:four){
			System.out.println(element.toString());
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
		List<Unit> five =cfg.getUnexceptionalSuccsOf(four.get(0));
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
		System.out.println(five.size());
	}
	
}
