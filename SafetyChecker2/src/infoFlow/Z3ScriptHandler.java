package infoFlow;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

import soot.ArrayType;
import soot.IntegerType;
import soot.Local;
import soot.PrimType;
import soot.RefLikeType;
import soot.RefType;
import soot.Type;
import soot.Unit;
import soot.Value;
import soot.jimple.AddExpr;
import soot.jimple.AnyNewExpr;
import soot.jimple.ArrayRef;
import soot.jimple.AssignStmt;
import soot.jimple.BinopExpr;
import soot.jimple.CastExpr;
import soot.jimple.Constant;
import soot.jimple.EqExpr;
import soot.jimple.FieldRef;
import soot.jimple.GeExpr;
import soot.jimple.GotoStmt;
import soot.jimple.GtExpr;
import soot.jimple.IdentityStmt;
import soot.jimple.IfStmt;
import soot.jimple.InstanceFieldRef;
import soot.jimple.IntConstant;
import soot.jimple.InvokeStmt;
import soot.jimple.LeExpr;
import soot.jimple.LtExpr;
import soot.jimple.NeExpr;
import soot.jimple.NewArrayExpr;
import soot.jimple.NewExpr;
import soot.jimple.NewMultiArrayExpr;
import soot.jimple.NullConstant;
import soot.jimple.StaticFieldRef;
import soot.jimple.StringConstant;
import soot.jimple.internal.JNewExpr;
import soot.shimple.PhiExpr;
import soot.toolkits.scalar.ValueUnitPair;

public class Z3ScriptHandler {
	
	private InterpolationContext ictx;
	private Map<String, Sort> newSortMap = new HashMap<String, Sort>();
	private Map<String, NewSort> sortId = new HashMap<String, NewSort>();
	private Map<String, Expr> global = new HashMap<String, Expr>();
	private Map<String, Expr> localMap = new HashMap<String, Expr>();
	private Map<String, Integer> arrayNameMap = new HashMap<String, Integer>();
	private Map<String, String> substitute = new HashMap<String, String>();
	private Map<String, Sort> substituteSort = new HashMap<String, Sort>();
	private Stack<Expr> parameters = new Stack<Expr>();
	private Vertex errorPathRoot;

	public Z3ScriptHandler(InterpolationContext ictx) {
		this.ictx = ictx;
	}
	
	public void convertPathtoZ3Script(Vertex v) {
		LogUtils.debugln(">>>>>>> Z3ScriptHandler.convertPathtoZ3Script");
		errorPathRoot = v;
		boolean isError = false;
		while(v != null) {
			if(isError) break;
			LogUtils.debugln(v.getOutgoingEdge());
			createZ3Script(v.getOutgoingEdge());
			v = v.getNextVertex();
			if(v.getOutgoingEdge().isErrorEdge())
				isError = true;
		}
		LogUtils.debugln("<<<<<<<< Z3ScriptHandler.convertPathtoZ3Script");
	}

	public boolean createZ3Script(Edge e) {
		boolean converted = false;
		if(e.isErrorEdge()) converted = convertErrorEdge(e); 
		Unit stmt = e.getUnit();
		if(stmt instanceof IfStmt) converted = convertIfStmt(e);
		if(stmt instanceof GotoStmt) converted = convertGotoStmt(e); 
		if(stmt instanceof AssignStmt) converted = convertAssignStmtEdge(e);
		// add invoke
		if(stmt instanceof IdentityStmt) converted = convertIdentityStmt(e);
		if(e.isSinkEdge()) converted = convertSinkInvoke2Z3(e);
	
		LogUtils.debugln("---------------");	
		LogUtils.debugln("Vertex=" + e.getSource() + "---- Unit=" + e);
		LogUtils.debugln("Expr" + e.getZ3Expr());
		if(!converted) {
			LogUtils.fatalln("Converstion failed");
			LogUtils.fatalln("Z3ScriptHandler.createZ3Script");
			System.exit(0);
		}
		return converted;
	}

	private boolean convertErrorEdge(Edge edge) {
		InvokeStmt errorInvoke = (InvokeStmt) edge.getUnit();
		return true;
	}

	private boolean convertIfStmt(Edge edge) {
		IfStmt ifStmt = (IfStmt) edge.getUnit();
		Value value = ifStmt.getCondition();
		BoolExpr condition = (BoolExpr) convertValue(value, false, edge, edge.getSource().getDistance());
		LogUtils.debugln(value);
	       	// check whether it is last in path
		Edge nextEdge = edge.getTarget().getOutgoingEdge();

		Unit currentUnit = edge.getUnit();
		LogUtils.debugln("currentUnit=" + currentUnit);
		Unit nextUnit = nextEdge.getUnit();
		LogUtils.debugln("nextUnit=" + nextUnit);
		Unit targetUnit = ifStmt.getTarget();
		LogUtils.debugln("targetUnit = " + targetUnit);

		if(targetUnit.equals(nextUnit)) 
			edge.setZ3Expr(condition);
		else
			edge.setZ3Expr(this.ictx.mkNot(condition));
		LogUtils.debugln(edge.getUnit() + "=" + edge.getZ3Expr()); 
		return true;
	}

	private boolean convertIdentityStmt(Edge edge) { 
		IdentityStmt iStmt = (IdentityStmt) edge.getUnit();
		Value left = iStmt.getLeftOp();
		Expr leftZ3 = convertValue(left, true, edge, edge.getSource().getDistance());
		if(parameters.isEmpty()) {
			Type t = left.getType();
			if(t instanceof RefType) {
				RefType rType = (RefType) t;
				NewExpr right = new JNewExpr(rType);
				Expr rightZ3 = convertValue(right, false, edge, 0);
				Type leftType = left.getType();
				BoolExpr expr = convertAssignStmt(rightZ3, leftZ3, leftType, left, edge.getSource().getDistance());
				edge.setZ3Expr(expr);
				if(expr == null) 
					return false;
				return true;	
			}
		} else {
			return false;
		}
		return false;
		
	}	

	private boolean convertAssignStmtEdge(Edge e) {
		AssignStmt aStmt = (AssignStmt) e.getUnit();
		Value left = aStmt.getLeftOp();
		Value right = aStmt.getRightOp();
		//nonsense and dummy parts needs to be added
		
		Type leftType = left.getType();
		Expr rightZ3 = null;
		// rigth invoke expression needs to be added
		rightZ3 = convertValue(right, false, e, e.getSource().getDistance());
		Expr leftZ3 = convertValue(left, true, e, e.getSource().getDistance());

		LogUtils.debugln(rightZ3);
		LogUtils.debugln(leftType);
		BoolExpr eq = convertAssignStmt(rightZ3, leftZ3, leftType, left, e.getSource().getDistance());
		e.setZ3Expr(eq);
		if(eq == null)
			return false;
		return true;

	}

	private boolean convertGotoStmt(Edge e) {
		e.setZ3Expr(this.ictx.mkTrue());
		return true;
	}

	private boolean convertSinkInvoke2Z3(Edge e) {
		String sign = UnitController.getMethodSignature(e.getUnit());
		Value leakCandidate = null;
		for(String sinkSignature : UnitController.sinkSignatureDB)	
			if(sign.contains(sinkSignature)) 
				leakCandidate = ((InvokeStmt)e.getUnit()).getInvokeExpr().getArg(UnitController.sensitiveParameterMap.get(sinkSignature)); 

		LogUtils.infoln("Unit : " + e.getUnit());
		LogUtils.infoln("leakCandidate : " + leakCandidate);
//		Expr leakCandidateZ3 = convertValue(leakCandidate, e);	
		return false;
	}

	private Expr convertValue(Value value, boolean assignLeft, Edge edge, int nodeIndex) {
		
		Type type = value.getType();
		if(type instanceof PrimType) {
			return convertPrimitiveValue(value, assignLeft, edge, nodeIndex);
		}
		if(type instanceof RefLikeType) 
			return convertRefLikeValue(value, assignLeft, edge, nodeIndex);
		return null;
	}

	private Expr convertPrimitiveValue(Value value, boolean assignLeft, Edge edge, int nodeIndex) {
		if(value instanceof Local) { 
			Local local = (Local) value;
			String oldName = local.getName();
			if(assignLeft) {
				Type type = value.getType();
				String newName = oldName + getNameSuffix(edge);
				Expr leftExpr = null;
				if(type instanceof IntegerType) {
					leftExpr = ictx.mkIntConst(newName);
					substituteSort.put(newName, ictx.mkIntSort());
				}
				localMap.put(oldName, leftExpr);
				return leftExpr;
			} else 
				return localMap.get(oldName);
		}
		if(value instanceof BinopExpr) {
			return convertBoolExpr((BinopExpr) value, edge, nodeIndex);
		}
		if(value instanceof Constant) {
			Constant constant = (Constant) value;
			if(constant instanceof IntConstant) {
				IntConstant intConstant = (IntConstant) constant;
				int intValue = intConstant.value;
				Expr exprValue = ictx.mkInt(intValue);
				return exprValue;
			}
		}
		if(value instanceof PhiExpr) {
			PhiExpr phiExpr = (PhiExpr) value;
			List<ValueUnitPair> pairList = phiExpr.getArgs();
			
			Vertex vertex = edge.getSource();
			Edge resultEdge = null;
			Value resultValue = null;
			boolean shortestResultFound = false;

			for(ValueUnitPair pair : pairList) {
//				if(resultEdge!=null && resultEdge.getTarget().getDistance()-vertex.getDistance()==0)
//					break;

				Value valuePair = pair.getValue();
				LogUtils.detailln("valuePair=" + valuePair);
				Unit unitPair = pair.getUnit();
				LogUtils.detailln("unitPair=" + unitPair);
				
				Vertex phiEqualityVertex = errorPathRoot;
				while(phiEqualityVertex != edge.getSource()) {
					Unit phiEqualityUnit = phiEqualityVertex.getOutgoingEdge().getUnit();	
					if(phiEqualityUnit.equals(unitPair)) {

						if(resultEdge == null) {
							resultEdge = phiEqualityVertex.getOutgoingEdge();
							resultValue = valuePair;
						} else if(phiEqualityVertex.getDistance() < resultEdge.getSource().getDistance()) {
							resultEdge = phiEqualityVertex.getOutgoingEdge();
							resultValue = valuePair;
						}
						LogUtils.detailln("phiEqualityUnit=" + phiEqualityUnit + "-- Dist-" + phiEqualityVertex.getDistance());
						LogUtils.detailln("resultunit=" + resultEdge + " -- Dis=" + resultEdge.getSource().getDistance());
					}
					phiEqualityVertex = phiEqualityVertex.getNextVertex();
					
				}
					
			}

			Expr resultExpr = convertValue(resultValue, false, edge, edge.getSource().getDistance());
			LogUtils.debugln("resultExpr=" + resultExpr);
			return resultExpr;
		}
		LogUtils.fatalln("returning null");
		LogUtils.fatalln("Vertex=" + edge.getSource() + "---Edge=" + edge);
		return null;
	}

	private Expr convertRefLikeValue(Value value, boolean assignLeft, Edge edge, int nodeIndex) {
		if(value instanceof PhiExpr) {
			LogUtils.fatalln("FATAL: PhiExpr is not supported yet!");
			System.exit(0);
		}
		if(value instanceof Local) {
			Type type = value.getType();		
			Local local = (Local) value;
			if(type instanceof RefType) {
				return createZ3Object(local, assignLeft,  edge);	
			}
			if(type instanceof ArrayType) {
				LogUtils.fatalln("FATAL: ArrayType is not supported yet!");
				System.exit(0);
			}
		}
		if(value instanceof AnyNewExpr) {
			return convertAnyNewExpr((AnyNewExpr) value, edge);	
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

	private Expr createZ3Object(Local v,  boolean IfAssignLeft, Edge e) {
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
		if (!global.containsKey(TypeName)) {
			Sort newArraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
			String arrayName = getGlobalName(TypeName);
			Expr newArray = ictx.mkConst(arrayName, newArraySort);
			global.put(TypeName, newArray); //theCoverter.updateGlobal(TypeName, newArray);
			substitute.put(TypeName, arrayName);
			NewSort s = new NewSort(newSort, ictx);
			sortId.put(TypeName, s);
		}
		if (IfAssignLeft) {
			String valueName = v.getName() + getNameSuffix(e);
			Expr a = ictx.mkConst(valueName, newSortMap.get(TypeName));
			NewSort s2 = sortId.get(TypeName);
			if (s2.ifHasExpr(a)) {
				return a;
			} else {
				s2.creatNewOject(a);
				return a;
			}
		} else {
			LogUtils.fatalln("Z3ScriptHandler.createZ3Object");	
			System.exit(0);
			ArrayExpr oldArray = (ArrayExpr) global.get(TypeName);
			NewSort s2 = sortId.get(TypeName);
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
		}
	}

	private String getGlobalName(String name) {
		String globalName = null;
		int index = 1;
		if(arrayNameMap.containsKey(name)) {
			index = arrayNameMap.get(name);
			index++;
		}
//		} else 
//			index = 1;
		globalName = "Global_" + name + "_" + index;
		arrayNameMap.put(name, index);
		return globalName;
	}	

	private BoolExpr convertAssignStmt(Expr rightZ3, Expr leftZ3, Type leftType, Value left, int i) {
		if ((leftType instanceof PrimType) && (left instanceof Local)) {
			BoolExpr leftEqRight = ictx.mkEq(leftZ3, rightZ3);
			return leftEqRight;
		}
		if ((leftType instanceof ArrayType) && (left instanceof Local)) {
			throw new RuntimeException();
//			String typeName = leftType.toString();
//			String virtualName = typeName + "Path" + theCoverter.getPathNumber() + "level" + level;
//			String newName = virtualName + "index" + i;
//			ArrayExpr latestArray = (ArrayExpr) this.latestLocal.get(virtualName);
//			ArrayExpr newArray = (ArrayExpr) this.theCoverter.getIctx().mkConst(newName, latestArray.getSort());
//			theCoverter.addSubstitute(newName, virtualName);
//			theCoverter.updateSubstituteSort(newName, newArray.getSort());
//			if (this.latestLocal.containsKey(virtualName)) {
//				this.latestLocal.remove(virtualName);
//				this.latestLocal.put(virtualName, newArray);
//			} else {
//				this.latestLocal.put(virtualName, newArray);
//			}
//			String sortName = typeName + "virtual" + "level" + level;
//			NewSort s = this.theCoverter.getSortId().get(sortName);
//			Expr afterStore = iCtx.mkStore((ArrayExpr) latestArray,
//			s.getId(leftZ3), rightZ3);
//			BoolExpr newArrayEqOldArray = iCtx.mkEq(newArray, afterStore);
//			return newArrayEqOldArray;
		}
		if (left instanceof ArrayRef) {
			throw new RuntimeException();
//			ArrayRef leftV = (ArrayRef) left;
//			return ArrayHelper.updateArrayRef(leftV, theCoverter, this, rightZ3);
		} else {
			String oldName = getArrayName(left);
			String newName = getGlobalName(oldName);
			Expr latestArray = global.get(oldName);
			Expr newArray = ictx.mkConst(newName, latestArray.getSort());
			substitute.put(newName, oldName);
			global.put(oldName, newArray);
			NewSort s = sortId.get(oldName);
			Expr afterStore = null;

			if ((left instanceof FieldRef) && (!(left instanceof StaticFieldRef))) 
				afterStore = ictx.mkStore((ArrayExpr) latestArray, leftZ3, rightZ3);
			else 
				afterStore = ictx.mkStore((ArrayExpr) latestArray, s.getId(leftZ3), rightZ3);
		
			BoolExpr newArrayEqOldArray = ictx.mkEq(newArray, afterStore);
			return newArrayEqOldArray;
		}	
	}

	private Expr convertAnyNewExpr(AnyNewExpr ane, Edge e) {
		if(ane instanceof NewExpr) return convertNewExpr((NewExpr)ane, e);
		if(ane instanceof NewArrayExpr) return convertNewArrayExpr((NewArrayExpr)ane, e);
		if(ane instanceof NewMultiArrayExpr) return convertNewMultiArrayExpr((NewMultiArrayExpr)ane, e);
		return null;
	}
	
	private Expr convertNewExpr(NewExpr ne, Edge e) {
		Type t = ne.getType();
		String typeName = t.toString();
		if(sortId.containsKey(typeName)) {
			NewSort s = sortId.get(typeName);
			return s.getNewObject();
		} else 
			throw new RuntimeException();
	}

	private Expr convertNewArrayExpr(NewArrayExpr ne, Edge e) {
		throw new RuntimeException();
	}
	
	private Expr convertNewMultiArrayExpr(NewMultiArrayExpr ne, Edge e) {
		throw new RuntimeException();
	}

	private String getArrayName(Value leftOp) {
		Type t = leftOp.getType();
		if(leftOp instanceof Local) return t.toString();

		// RefHelper.getRrayname
		throw new RuntimeException();
	}

	private String getNameSuffix(Edge e) {
		return "_" + e.getProgramTree().getProgramDefinition() + "_" + e.getSource().getDistance();
	}

	private Expr convertBoolExpr(BinopExpr expr, Edge edge, int nodeIndex) {
		
		if(expr instanceof AddExpr) {
			AddExpr addExpr = (AddExpr) expr;

			Value op1Value = addExpr.getOp1();
			Value op2Value = addExpr.getOp2();

			Expr op1Expr = convertValue(op1Value, false, edge, edge.getSource().getDistance());
			Expr op2Expr = convertValue(op2Value, false, edge, edge.getSource().getDistance());

			return ictx.mkAdd((ArithExpr)op1Expr, (ArithExpr)op2Expr);
		}
		if(expr instanceof EqExpr) {
			EqExpr eqExpr = (EqExpr) expr;
			Value op1Value = eqExpr.getOp1();
			Value op2Value = eqExpr.getOp2();

			Expr op1Expr = convertValue(op1Value, false, edge, edge.getSource().getDistance());
			Expr op2Expr = convertValue(op2Value, false, edge, edge.getSource().getDistance());

			return ictx.mkEq(op1Expr, op2Expr);
		}
		if(expr instanceof NeExpr) {
			NeExpr neExpr = (NeExpr) expr;
			Value op1Value = neExpr.getOp1();
			Value op2Value = neExpr.getOp2();

			Expr op1Expr = convertValue(op1Value, false, edge, edge.getSource().getDistance());
			Expr op2Expr = convertValue(op2Value, false, edge, edge.getSource().getDistance());

			BoolExpr eqExpr = this.ictx.mkEq(op1Expr, op2Expr);
			return this.ictx.mkNot(eqExpr);
			
		}
		if(expr instanceof GtExpr) {
			GtExpr gtExpr = (GtExpr) expr;
			Value op1Value = gtExpr.getOp1();
			Value op2Value = gtExpr.getOp2();

			Expr op1Expr = convertValue(op1Value, false, edge, edge.getSource().getDistance());
			Expr op2Expr = convertValue(op2Value, false, edge, edge.getSource().getDistance());

			return  this.ictx.mkGt((ArithExpr)op1Expr, (ArithExpr)op2Expr);
		}
		if(expr instanceof GeExpr) {
			GeExpr geExpr = (GeExpr) expr;
			Value op1Value = geExpr.getOp1();
			Value op2Value = geExpr.getOp2();

			Expr op1Expr = convertValue(op1Value, false, edge, edge.getSource().getDistance());
			Expr op2Expr = convertValue(op2Value, false, edge, edge.getSource().getDistance());

			return  this.ictx.mkGe((ArithExpr)op1Expr, (ArithExpr)op2Expr);
		}
		if(expr instanceof LtExpr) {
			LtExpr ltExpr = (LtExpr) expr;
			Value op1Value = ltExpr.getOp1();
			Value op2Value = ltExpr.getOp2();

			Expr op1Expr = convertValue(op1Value, false, edge, edge.getSource().getDistance());
			Expr op2Expr = convertValue(op2Value, false, edge, edge.getSource().getDistance());

			return  this.ictx.mkLt((ArithExpr)op1Expr, (ArithExpr)op2Expr);
		}
		if(expr instanceof LeExpr) {
			LeExpr leExpr = (LeExpr) expr;
			Value op1Value = leExpr.getOp1();
			Value op2Value = leExpr.getOp2();

			Expr op1Expr = convertValue(op1Value, false, edge, edge.getSource().getDistance());
			Expr op2Expr = convertValue(op2Value, false, edge, edge.getSource().getDistance());

			return  this.ictx.mkLe((ArithExpr)op1Expr, (ArithExpr)op2Expr);
		}
		return null;
	}

}
