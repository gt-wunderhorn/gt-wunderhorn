package SelfCompositionTest;

import infoFlow.Forest;
import infoFlow.Node;
import infoFlow.Tree;

import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import soot.Body;
import soot.BodyTransformer;
import soot.Local;
import soot.PackManager;
import soot.SootMethod;
import soot.Transform;
import soot.Type;
import soot.Value;
import soot.options.Options;

public class NoLeak1 {
	static Map<String, Body> stores = new HashMap<String, Body>();

	public static void main(String[] args) throws FileNotFoundException,
			UnsupportedEncodingException {
		Options.v().set_src_prec(Options.src_prec_c);
		Options.v().set_output_format(Options.output_format_shimple);
		Options.v().set_allow_phantom_refs(true);
		String[] sootArgs = new String[] {
				"-process-dir",
				"/Users/burak/Documents/WHarris/droidsafe-src/android-apps/examples/DroidBech/ImplicitFlows/ImplicitFlow3_changed/bin/classes",
				"-output-dir", "src/output/SelfCompositionTest1" };
		PackManager.v().getPack("stp")
				.add(new Transform("stp.test", new BodyTransformer() {

					@Override
					protected void internalTransform(Body body,
							String phaseName, Map<String, String> options) {
						// hack here
						SootMethod method = body.getMethod();
						String methodSig = method.getSignature();
						System.out.println(methodSig);
						
						if(method.getName().equals("noLeak1"))
							System.out.println("huhu");
						/* System.out.println(method.getName()); */
//						if(method.getName().equals("onCreate")) {
//							System.out.println(method);
//							System.out.println(body.getLocals());
//							System.out.println(body.getParameterLocals());
//							System.out.println(body.getParameterRefs());
//							System.out.println(method.getParameterTypes());
//							List<Type>l = new LinkedList<Type>();
//							l.add(method.getParameterTypes().get(0));
//							l.add(method.getParameterTypes().get(0));
//							
//							
//							
//							method.setParameterTypes(l);
//							System.out.println(method);
//							System.out.println(body.getParameterRefs());
//							System.out.println(method.getParameterTypes());
//							//System.out.println(body.getTags());
//							//System.out.println(body.getThisLocal());
//							//System.out.println(body.getUnits());
//							
//							System.out.println(body.getThisLocal());
//							
//							//System.out.println("{{{{{{{{{{{iiiiii}}}}}}}}}}}}}");
//							Body body2 = (Body) body.clone();
//							Iterator<Local> it = body2.getLocals().iterator();
//							while(it.hasNext()) {
//								Local lcl = it.next();
//								lcl.setName(lcl.getName()+"_0");
//							}
//							//System.out.println(body2.getUnits());
//							//body.getUnits().addAll(body2.getUnits());
//							System.out.println(body);
//							//System.out.println(body2);
//							//System.exit(0);
//						}
						stores.put(methodSig, body);
						

					}
				}));
		soot.Main.main(sootArgs);
		System.out.println(stores.size());
		Forest forest1 = new Forest(stores,
				"<de.ecspride.ImplicitFlow3: void tester()>", "<de.ecspride.ImplicitFlow3: void onCreate2(android.os.Bundle)>");
				//"<de.ecspride.ImplicitFlow3: void onCreate(android.os.Bundle)>");
		forest1.TestCorrect();
		Tree theMain = forest1.getMainTree();
		if (forest1.ifCorrect()) {
			System.out.println("there is no leak");
		} else {
			System.out.println("there is a leak");
		}
	}
}
