package infoFlow;

import java.util.HashMap;
import java.util.Map;

import com.microsoft.z3.Expr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

import soot.ArrayType;
import soot.Local;
import soot.PrimType;
import soot.RefLikeType;
import soot.RefType;
import soot.Type;
import soot.Value;
import soot.jimple.AnyNewExpr;
import soot.jimple.ArrayRef;
import soot.jimple.CastExpr;
import soot.jimple.InstanceFieldRef;
import soot.jimple.InvokeStmt;
import soot.jimple.NullConstant;
import soot.jimple.StaticFieldRef;
import soot.jimple.StringConstant;
import soot.shimple.PhiExpr;

public class InterpolationHandler {
	
	private InterpolationContext ictx;
	private Map<String, Sort> newSortMap = new HashMap<String, Sort>();

	public InterpolationHandler() {
		ictx = new InterpolationContext();
	}
	
	public boolean createZ3Script(Edge e) {
		if(e.isSinkEdge()) convertSinkInvoke2Z3(e);
		
		


		return false;
	}

	private boolean convertSinkInvoke2Z3(Edge e) {
		String sign = UnitController.getMethodSignature(e.getUnit());
		Value leakCandidate = null;
		for(String sinkSignature : UnitController.sinkSignatureDB)	
			if(sign.contains(sinkSignature)) 
				leakCandidate = ((InvokeStmt)e.getUnit()).getInvokeExpr().getArg(UnitController.sensitiveParameterMap.get(sinkSignature)); 

		LogUtils.infoln("Unit : " + e.getUnit());
		LogUtils.infoln("leakCandidate : " + leakCandidate);
		Expr leakCandidateZ3 = convertValue(leakCandidate, e);	




		return false;
	}

	private Expr convertValue(Value value, Edge edge) {
		
		Type type = value.getType();
		if(type instanceof PrimType) {
			LogUtils.fatalln("PrimitiveType");
			System.exit(0);
		}
		if(type instanceof RefLikeType) 
			convertRefLikeValue(value, edge);
		return null;
	}

	private Expr convertRefLikeValue(Value value, Edge edge) {
		if(value instanceof PhiExpr) {
			LogUtils.fatalln("FATAL: PhiExpr is not supported yet!");
			System.exit(0);
		}
		if(value instanceof Local) {
			Type type = value.getType();		
			Local local = (Local) value;
			if(type instanceof RefType) {
				return createZ3Object(local, edge);	
			}
			if(type instanceof ArrayType) {
				LogUtils.fatalln("FATAL: ArrayType is not supported yet!");
				System.exit(0);
			}
		}
		if(value instanceof AnyNewExpr) {
			LogUtils.fatalln("FATAL: AnyNewExpr is not supported yet!");
			System.exit(0);
		}
		if(value instanceof StringConstant) {
			LogUtils.fatalln("FATAL: StringConstant. is not supported yet!");
			System.exit(0);
		}
		if(value instanceof ArrayRef) {
			LogUtils.fatalln("FATAL: ArrayRef is not supported yet!");
			System.exit(0);
		}
		if(value instanceof InstanceFieldRef) {
			LogUtils.fatalln("FATAL: InstanceFieldRef is not supported yet!");
			System.exit(0);
		}
		if(value instanceof CastExpr) {
			LogUtils.fatalln("FATAL: CastExpr is not supported yet!");
			System.exit(0);
		}
		if(value instanceof StaticFieldRef) {
			LogUtils.fatalln("FATAL: StaticFieldRef is not supported yet!");
			System.exit(0);
		}
		if(value instanceof NullConstant) {
			LogUtils.fatalln("FATAL: NullConstant is not supported yet!");
			System.exit(0);
		}
			
		LogUtils.fatalln("FATAL: Conversion cannot be done");
		LogUtils.fatalln("FATAL: Unit : " + edge.getUnit() + " - Value : " + value);
		return null;
	}

	private Expr createZ3Object(Local v /*,  boolean IfAssignLeft */, Edge e) {
		Type type = v.getType();
		String TypeName = type.toString();
		Sort newSort = null;
//		Map<String, NewSort> sortId = theCoverter.getSortId();
		if (newSortMap.containsKey(TypeName)) {
			newSort = newSortMap.get(TypeName);
		} else {
      		newSort = ictx.mkUninterpretedSort(ictx.mkSymbol(TypeName));
			newSortMap.put(TypeName, newSort);
		}
//		if (!theCoverter.hasGlobal(TypeName)) {
//			Sort newArraySort = theCoverter.getIctx().mkArraySort(
//			theCoverter.getIctx().getIntSort(),
//			theCoverter.getIctx().getIntSort());
//			String arrayName = theCoverter.getGlobalName(TypeName);
//			Expr newArray = theCoverter.getIctx().mkConst(arrayName,
//			newArraySort);
//			theCoverter.updateGlobal(TypeName, newArray);
//			theCoverter.addSubstitute(TypeName, arrayName);
//			NewSort s = new NewSort(newSort, theCoverter);
//			sortId.put(TypeName, s);
//		}
//		if (IfAssignLeft) {
//			String valueName = v.getName() + theCoverter.getRenameString(level);
//			Expr a = theCoverter.getIctx().mkConst(valueName,
//			newSortMap.get(TypeName));
//			NewSort s2 = sortId.get(TypeName);
//			if (s2.ifHasExpr(a)) {
//				return a;
//			} else {
//				s2.creatNewOject(a);
//				return a;
//			}
//		} else {
//			ArrayExpr oldArray = (ArrayExpr) theCoverter.getGlobal(TypeName);
//			NewSort s2 = sortId.get(TypeName);
			String valueName = v.getName() + e.getProgramTree().getProgramDefinition(); 
			Expr a = ictx.mkConst(valueName, newSort);// newSortMap.get(TypeName));
//			System.out.println(v);
			//Expr result = theCoverter.getIctx().mkSelect(oldArray, s2.getId(a));
			//LogUtils.infoln("result=" + result);
			LogUtils.infoln(valueName);
			LogUtils.infoln(newSort);
			LogUtils.infoln(a);
			System.exit(0);
			return null;
			//return result;
//		}
	}

}
