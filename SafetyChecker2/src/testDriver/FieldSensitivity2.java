package testDriver;

import infoFlow.Forest;
import infoFlow.Node;
import infoFlow.Tree;

import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import soot.Body;
import soot.BodyTransformer;
import soot.Local;
import soot.PackManager;
import soot.SootMethod;
import soot.Transform;
import soot.Unit;
import soot.Value;
import soot.jimple.AssignStmt;
import soot.jimple.GotoStmt;
import soot.jimple.IfStmt;
import soot.jimple.internal.JimpleLocal;
import soot.options.Options;
import soot.toolkits.graph.ExceptionalUnitGraph;

public class FieldSensitivity2 {
	static Map<String,Body> stores=new HashMap<String,Body>();
	public static void main(String[] args) throws FileNotFoundException, UnsupportedEncodingException {
		Options.v().set_src_prec(Options.src_prec_c);
		Options.v().set_output_format(Options.output_format_shimple);
		Options.v().set_allow_phantom_refs(true);
		String[] sootArgs=new String[]{
			"-process-dir","C:/Users/qizhou/workspace/InfoFlow1/BechMark/FieldSensitivity2/bin/classes",
			"-output-dir","src/output/FieldSensitivity2"
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
		Forest forest1 = new Forest(stores,
				"<de.ecspride.FieldSensitivity2: void onCreate(android.os.Bundle)>");
		forest1.TestCorrect();
		Tree theMain=forest1.getMainTree();
		if(forest1.ifCorrect()){
			System.out.println("there is no leak");
		}
		else{
			System.out.println("there is a leak");
		}
		System.out.println(theMain.getAllErrorPath().size());
		ArrayList<Node> errorPath=theMain.getAllErrorPath().get(0);
	}
}
