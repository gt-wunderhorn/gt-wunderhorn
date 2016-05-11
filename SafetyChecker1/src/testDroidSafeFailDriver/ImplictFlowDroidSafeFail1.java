package testDroidSafeFailDriver;

import infoFlow.Forest;
import infoFlow.Tree;

import java.awt.List;
import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryPoolMXBean;
import java.lang.management.MemoryUsage;
import java.util.HashMap;
import java.util.Map;

import soot.Body;
import soot.BodyTransformer;
import soot.PackManager;
import soot.SootMethod;
import soot.Transform;
import soot.options.Options;

public class ImplictFlowDroidSafeFail1 {
	static Map<String, Body> stores = new HashMap<String, Body>();

	public static void main(String[] args) throws FileNotFoundException,
			UnsupportedEncodingException {
		Options.v().set_src_prec(Options.src_prec_c);
		Options.v().set_output_format(Options.output_format_shimple);
		Options.v().set_allow_phantom_refs(true);
		String[] sootArgs = new String[] {
				"-process-dir",
				"/Users/qizhou/Documents/workspace/InfoFlow1/BechMark/DroidSafeFail/ImplictFlowDroidSafeFail1/bin/classes",
				"-output-dir", "src/output/DroidSafeFail/GTFlowTest" };
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
		Forest forest1 = new Forest(stores,
				"<com.flow.ImplicitFlow1: void onCreate(android.os.Bundle)>");
		forest1.TestCorrect();
		if (forest1.ifCorrect()) {
			System.out.println("there is no leak");
		} else {
			System.out.println("there is a leak");
		}
		
		
	}
}
