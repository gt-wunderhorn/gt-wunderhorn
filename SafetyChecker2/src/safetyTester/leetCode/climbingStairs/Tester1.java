package safetyTester.leetCode.climbingStairs;

import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;

import safetyChecker.LogUtils;
import safetyChecker.ProgramTree;

import soot.Body;
import soot.BodyTransformer;
import soot.PackManager;
import soot.SootMethod;
import soot.Transform;
import soot.options.Options;

public class Tester1 {
	static Map<String, Body> stores = new HashMap<String, Body>();

	public static void main(String[] args) throws FileNotFoundException,
			UnsupportedEncodingException {
		Options.v().set_src_prec(Options.src_prec_c);
		Options.v().set_output_format(Options.output_format_shimple);
		Options.v().set_allow_phantom_refs(true);
		String[] sootArgs = new String[] {
				"-process-dir",
				"/Users/burak/Documents/WHarris/safety-itps/SafetyChecker2/bin",
				"-output-dir", "src/output/safetyTest" };
		PackManager.v().getPack("stp")
				.add(new Transform("stp.test", new BodyTransformer() {

					@Override
					protected void internalTransform(Body body,
							String phaseName, Map<String, String> options) {
						// hack here
						SootMethod method = body.getMethod();
						String methodSig = method.getSignature();
						//System.out.println(methodSig);
						
						stores.put(methodSig, body);
					}
				}));
		soot.Main.main(sootArgs);
		//System.out.println(stores.size());

		String mainFunction ="<safetyTestCode.leetCode.climbingStairs.Test1: int climbStairs(int)>";
		LogUtils.infoln("safetyTester.leetCode.climbingStairs.Tester1... Test started...");
	
		try {	
			ProgramTree pTree = new ProgramTree(stores, mainFunction, true);
			pTree.printResult(mainFunction);

		} catch (Exception exp){
			LogUtils.fatalln("*******************************************");
			LogUtils.fatalln("*********** EXCEPTION OCCURRED ************");
			LogUtils.fatalln("*******************************************");
			LogUtils.fatalln(exp);
			LogUtils.fatalln("******************"); 
		//	LogUtils.fatalln(exp.getStackTrace());
			LogUtils.fatalln("******************"); 
			exp.printStackTrace();
		}

		LogUtils.infoln("safetyTester.leetCode.climbingStairs.Tester1... Test finished...");
		/// There will no need to forest anymore....
//		Forest forest1 = new Forest(stores,
//				"<de.ecspride.ImplicitFlow3: void tester()>", "<de.ecspride.ImplicitFlow3: void onCreate2(android.os.Bundle)>");
//				//"<de.ecspride.ImplicitFlow3: void onCreate(android.os.Bundle)>");
//		forest1.TestCorrect();
//		Tree theMain = forest1.getMainTree();
		
		//create my tree over here...
		
		
//		if (forest1.ifCorrect()) {
//			System.out.println("there is no leak");
//		} else {
//			System.out.println("there is a leak");
//		}
	}
}
