package safetyChecker.z3ScriptManager;

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

import safetyChecker.Edge;
import safetyChecker.NewSort;
import safetyChecker.UnitController;
import safetyChecker.Vertex;
import safetyChecker.utilities.LogUtils;

import soot.ArrayType;
import soot.Body;
import soot.IntegerType;
import soot.Local;
import soot.LongType;
import soot.PrimType;
import soot.RefLikeType;
import soot.RefType;
import soot.SootField;
import soot.Type;
import soot.Unit;
import soot.Value;
import soot.ValueBox;
import soot.VoidType;
import soot.jimple.AddExpr;
import soot.jimple.AnyNewExpr;
import soot.jimple.ArrayRef;
import soot.jimple.AssignStmt;
import soot.jimple.BinopExpr;
import soot.jimple.CastExpr;
import soot.jimple.CmpExpr;
import soot.jimple.Constant;
import soot.jimple.DivExpr;
import soot.jimple.EqExpr;
import soot.jimple.FieldRef;
import soot.jimple.GeExpr;
import soot.jimple.GotoStmt;
import soot.jimple.GtExpr;
import soot.jimple.IdentityStmt;
import soot.jimple.IfStmt;
import soot.jimple.InstanceFieldRef;
import soot.jimple.IntConstant;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;
import soot.jimple.LeExpr;
import soot.jimple.LongConstant;
import soot.jimple.LtExpr;
import soot.jimple.MulExpr;
import soot.jimple.NeExpr;
import soot.jimple.NewArrayExpr;
import soot.jimple.NewExpr;
import soot.jimple.NewMultiArrayExpr;
import soot.jimple.NullConstant;
import soot.jimple.ParameterRef;
import soot.jimple.ReturnStmt;
import soot.jimple.StaticFieldRef;
import soot.jimple.StringConstant;
import soot.jimple.SubExpr;
import soot.jimple.VirtualInvokeExpr;
import soot.jimple.internal.JNewArrayExpr;
import soot.jimple.internal.JNewExpr;
import soot.jimple.internal.JimpleLocal;
import soot.shimple.PhiExpr;
import soot.toolkits.scalar.ValueUnitPair;

public class Z3ScriptHandler {
	
	private InterpolationContext ictx;
	private Map<String, Body> stores;
	private Map<String, Sort> newSortMap = new HashMap<String, Sort>();
	private Map<String, NewSort> sortId = new HashMap<String, NewSort>();
	private Map<String, Expr> global = new HashMap<String, Expr>();
//	private Map<String, Expr> localMap = new HashMap<String, Expr>();
	private Map<String, Map<String, Expr>> localMap = new HashMap<String, Map<String, Expr>>();
	private Map<String, Integer> arrayNameMap = new HashMap<String, Integer>();
	private Map<String, Integer> realArraySize = new HashMap<String, Integer>();
	private Map<String, Integer> maxArraySize = new HashMap<String, Integer>();
	private Map<String, String> substitute = new HashMap<String, String>();
	private Map<String, Sort> substituteSort = new HashMap<String, Sort>();
	private Stack<Expr> parameters = new Stack<Expr>();
	private Z3ArrayHandler arrayHandler = new Z3ArrayHandler();
	private Z3ObjectFieldHandler objFieldHandler = new Z3ObjectFieldHandler();
	private Z3JavaMathLibrary z3MathLibrary = new Z3JavaMathLibrary();
	private Z3JavaStringLibrary z3StringLibrary = new Z3JavaStringLibrary();
	private Vertex errorPathRoot;
	private Edge currentEdge;

	public Z3ScriptHandler(InterpolationContext ictx, Map<String, Body> stores) {
		this.ictx = ictx;
		this.stores = stores;
	}
	
	public void convertPathtoZ3Script(Vertex v) {
		LogUtils.debugln(">>>>>>> Z3ScriptHandler.convertPathtoZ3Script");
		errorPathRoot = v;
		boolean isError = false;
		while(v != null) {
			if(isError) break;
			LogUtils.debug("^^^^^");
			LogUtils.debugln(v + " *** " + v.getOutgoingEdge() + "***" + v.getOutgoingEdge().getSource() + "***" + v.getNextVertex() + "***" + v.getOutgoingEdge().getTarget());
			createZ3Script(v.getOutgoingEdge());
	
			v = v.getNextVertex();
			if(v == null) LogUtils.warningln("NUUUUUULLLLLLL");
			if(v.getOutgoingEdge().isErrorEdge())
				isError = true;
		}
		LogUtils.debugln("<<<<<<<< Z3ScriptHandler.convertPathtoZ3Script");
	}

	public boolean createZ3Script(Edge e) {
		LogUtils.warning(">>>>>>");
		LogUtils.infoln(e.getSource() + "***" + e);
		boolean converted = false;
		currentEdge = e;
		if(e.isErrorEdge()) converted = convertErrorEdge(e); 
		Unit stmt = e.getUnit();
		if(stmt instanceof IfStmt) converted = this.convertIfStmt(e);
		if(stmt instanceof GotoStmt) converted = this.convertGotoStmt(e); 
		if(stmt instanceof AssignStmt) converted = this.convertAssignStmtEdge(e);
		// add invoke
		if(stmt instanceof IdentityStmt) converted = this.convertIdentityStmt(e);
		if(stmt instanceof InvokeStmt && !e.isFunctionCall()) converted = convertNotSubFuntionInvoke(e);
		if(e.isSinkEdge()) converted = convertSinkInvoke2Z3(e);
		if(e.isArrayCopyEdge()) converted = convertArrayCopy(e);
		if(e.isNewString()) converted = convertNewStringExpr(e);
		if(stmt instanceof ReturnStmt) converted = convertReturnStmt(e);
		if(e.isFunctionCall() && !(stmt instanceof AssignStmt)) converted = this.convertFunctionCallOnly(e);
	
		LogUtils.infoln("z3Expr=" + e.getZ3Expr());
		if(!converted) {
			LogUtils.warningln("---------------");	
			LogUtils.warningln("Vertex=" + e.getSource() + "---- Unit=" + e);
			LogUtils.warningln("Expr=" + e.getZ3Expr());
			LogUtils.fatalln("Converstion failed");
			LogUtils.warningln("type  fo the " + e.getZ3Expr() + " is " + e.getUnit().getClass().getName());
			LogUtils.fatalln("Z3ScriptHandler.createZ3Script");
		}
		return converted;
	}

	private boolean convertFunctionCallOnly(Edge edge) {
		edge.setZ3Expr(this.ictx.mkTrue());
		return true;
	}

	private boolean convertReturnStmt(Edge edge) {
		edge.getProgramTree().getCallerVertex().getOutgoingEdge().setFunctionReturn(edge);
		edge.setZ3Expr(this.ictx.mkTrue());
		return true;
	}
	
	private boolean convertNewStringExpr(Edge edge) {
		InvokeStmt iStmt = (InvokeStmt) edge.getUnit();
		InvokeExpr iExpr = iStmt.getInvokeExpr();
		StringConstant sc = (StringConstant) iExpr.getArg(0);
		ValueBox valueBox = iExpr.getUseBoxes().get(1);
		Value callerObject = valueBox.getValue();
		Expr rightZ3 = convertValue(callerObject, true, edge, 0);
		BoolExpr f1 = Z3StringHandler.covertString(sc, this, rightZ3);
		LogUtils.fatalln(rightZ3);
		LogUtils.fatalln(f1);
		BoolExpr eq = this.ictx.mkEq(rightZ3, f1); 
		edge.setZ3Expr(eq);
		return true;
	}

	private boolean convertNotSubFuntionInvoke(Edge edge){
		edge.setZ3Expr(this.ictx.mkTrue());
		return true;
	}

	private boolean convertArrayCopy(Edge edge) {
		BoolExpr arrayCopyExpr = arrayHandler.z3ArrayCopy(edge, this);	
		edge.setZ3Expr(arrayCopyExpr);
		LogUtils.debugln("arrayCopy=" + arrayCopyExpr);
		if(arrayCopyExpr != null) 
			return true;
		return false;
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
		LogUtils.debugln("Z3ScriptHandler.convertIdentityStmt=" + edge);
		IdentityStmt iStmt = (IdentityStmt) edge.getUnit();
		Value left = iStmt.getLeftOp();
		Type leftType = left.getType();
		Expr leftZ3 = convertValue(left, true, edge, edge.getSource().getDistance());
		if(parameters.isEmpty()) {
			Type t = left.getType();
			if(t instanceof RefType) {
				RefType rType = (RefType) t;
				NewExpr right = new JNewExpr(rType);
				Expr rightZ3 = convertValue(right, false, edge, 0);
				BoolExpr expr = convertAssignStmt(rightZ3, leftZ3, leftType, left, edge.getSource().getDistance());
				edge.setZ3Expr(expr);
				if(expr == null) 
					return false;
				return true;	
			} else { 
				if(edge.getProgramTree().isMainFunction()) {
					edge.setZ3Expr(this.ictx.mkTrue());
					return true;
				}

				Value right = iStmt.getRightOp();
				if(right instanceof ParameterRef) {
					ParameterRef par = (ParameterRef) right;
					int parameterIndex = par.getIndex();

					Edge callerEdge = edge.getProgramTree().getCallerVertex().getOutgoingEdge();
					Unit callerUnit = callerEdge.getUnit();
					Expr rightZ3 = null;
					if(callerUnit instanceof AssignStmt) {
						AssignStmt aStmt = (AssignStmt) callerUnit;
						InvokeExpr iExpr = (InvokeExpr) aStmt.getRightOp();
						Value arg = iExpr.getArg(parameterIndex);
						rightZ3 = this.convertValue(arg, false, callerEdge, callerEdge.getSource().getDistance());  
						
					} else {
						InvokeStmt iStmt2 = (InvokeStmt) callerUnit; 
						Value arg = iStmt2.getInvokeExpr().getArg(parameterIndex);
						rightZ3 = this.convertValue(arg, false, callerEdge, callerEdge.getSource().getDistance());
					}	
					
					BoolExpr expr = this.convertAssignStmt(rightZ3, leftZ3, leftType, left, edge.getSource().getDistance());
					edge.setZ3Expr(expr);

				}

	//			edge.setZ3Expr(this.ictx.mkTrue());
				return true;
			}
		} else {
			LogUtils.fatalln("Z3ScriptHandler.convertIdentityStmt needs to be handled");
			return false;
		}
	}	

	private boolean convertAssignStmtEdge(Edge edge) {
		LogUtils.debugln("Z3ScriptHandler.convertAssignStmtEdge=" + edge.getSource() + "***"  + edge);
		AssignStmt aStmt = (AssignStmt) edge.getUnit();
		Value left = aStmt.getLeftOp();
		Value right = aStmt.getRightOp();
		//nonsense and dummy parts needs to be added
		
		if(left.getType() instanceof RefType && right instanceof VirtualInvokeExpr) {
			right = new JNewExpr((RefType) left.getType()); 
		} else if (right.toString().contains("java.lang.String[] split(java.lang.String)")) {
			right = new JNewArrayExpr(RefType.v("java.lang.String"), IntConstant.v(0)); 
		}
		LogUtils.debugln("right=" + right);
		
		Type leftType = left.getType();
		Expr rightZ3 = null;

		// rigth invoke expression needs to be added
		if(edge.isFunctionCall() && !(((InvokeExpr)right).getMethod().getReturnType() instanceof VoidType)) {
			LogUtils.fatal("******");
			LogUtils.infoln("subfunction call");
			Unit stmt = edge.getFunctionReturn().getUnit();
			LogUtils.fatal("******");
			LogUtils.debugln(stmt);
			Value returnValue = stmt.getUseBoxes().get(0).getValue();
			rightZ3 = this.convertValue(returnValue, false, edge.getFunctionReturn(), edge.getFunctionReturn().getSource().getDistance());
//			LogUtils.fatalln(right);
//			LogUtils.fatalln(((InvokeExpr)right).getMethod());
//			String subFuncSig = ((InvokeExpr)right).getMethod().toString();
//			try{
//				LogUtils.warningln("calls programtree");
//				Vertex returnRoot = null;
//				ProgramTree subPT = new ProgramTree(stores, subFuncSig, false);
//				if(subPT.getNewReturnPath())
//					returnRoot = subPT.getNewReturnRoot();
//				while(returnRoot != null)
//					LogUtils.warningln(returnRoot);
//
//			} catch (Exception e) {
//				LogUtils.warningln("ahahahahahahah");
//				LogUtils.fatalln(e.getMessage());
//				rightZ3 = this.ictx.mkIntConst("return_" + this.getRealArraySize("return_"));
//			}
		} else if(right instanceof InvokeExpr && !edge.isFunctionCall()) { 
			if(this.z3MathLibrary.isJavaMathLibrary(right))
				rightZ3 = this.z3MathLibrary.createMathEquality(right, this, edge);
			else if(this.z3StringLibrary.isJavaStringLibrary(right))
				rightZ3 = this.z3StringLibrary.createStringEquality(right, this, edge);
			else if(edge.isArrayCopyEdge() || UnitController.isArraysEqualsInvoke(right))
				rightZ3 = convertValue(right, false, edge, edge.getSource().getDistance());
			else
				rightZ3 = this.ictx.mkIntConst("nonSubFunction_" +this.getRealArraySize("nonSubFunction_"));
		} else {
			rightZ3 = convertValue(right, false, edge, edge.getSource().getDistance());
		}
		LogUtils.debugln("rightZ3=" + rightZ3);
		Expr leftZ3 = this.convertValue(left, true, edge, edge.getSource().getDistance());
		LogUtils.debugln("leftZ3=" + leftZ3);

		BoolExpr eq = null; 

		if(z3MathLibrary.isModulusInstruction(right))
			eq = z3MathLibrary.createModuleExpr(leftZ3, right, this, edge);
		else
			eq = convertAssignStmt(rightZ3, leftZ3, leftType, left, edge.getSource().getDistance());

		if(right instanceof AnyNewExpr) {
			if(right.getType().toString().equals("java.lang.String")) {
				BoolExpr f1 = Z3StringHandler.covertString(null, this, rightZ3);
				BoolExpr wholeFormula = this.ictx.mkAnd(eq, f1);
				edge.setZ3Expr(wholeFormula);
			} if(right instanceof NewArrayExpr) { 
				BoolExpr realArray = arrayHandler.newArrayExpr(rightZ3, right.getType(), this);
			        BoolExpr arrayExpr = this.ictx.mkAnd(eq, realArray);
				edge.setZ3Expr(arrayExpr);		
			} else if(right instanceof NewMultiArrayExpr) {
				BoolExpr realMulArray = arrayHandler.newMultiArrayExpr((NewMultiArrayExpr)right, right.getType(), this, rightZ3);
				BoolExpr arrayExpr = this.ictx.mkAnd(eq, realMulArray);
				edge.setZ3Expr(arrayExpr);
			} else if (right instanceof NewExpr) {
				edge.setZ3Expr(eq);	
			}
		}  else if(right instanceof StringConstant || (left.getType().equals("java.lang.String") && right.toString().contains("readLine"))) { 
			StringConstant sc = (StringConstant) right;
			BoolExpr f1 = Z3StringHandler.covertString(sc, this, rightZ3);
			BoolExpr wholeFormula = this.ictx.mkAnd(eq, f1);
			edge.setZ3Expr(wholeFormula);
		
		}  else {
			edge.setZ3Expr(eq);
		}
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
		return false;
	}

	protected Expr convertValue(Value value, boolean assignLeft, Edge edge, int nodeIndex) {
		
		Type type = value.getType();
		if(type instanceof PrimType) {
			return convertPrimitiveValue(value, assignLeft, edge, nodeIndex);
		}
		if(type instanceof RefLikeType) { 
			return convertRefLikeValue(value, assignLeft, edge, nodeIndex);
		}
		LogUtils.fatalln("not a primtype or a refliketype");
		return null;
	}

	private Expr convertPrimitiveValue(Value value, boolean assignLeft, Edge edge, int nodeIndex) {
		LogUtils.debugln("Z3ScriptHandler.convertPrimitiveValue=" + edge);
		if(value instanceof Local) { 
			Local local = (Local) value;
			String oldName = local.getName();
			String functionDefinition = edge.getProgramTree().getProgramDefinition();
			if(assignLeft) {
				Type type = value.getType();
				String newName = oldName + getNameSuffix(edge);
				Expr leftExpr = null;
				///
				if(!localMap.containsKey(functionDefinition)) {
					LogUtils.debugln(functionDefinition + " does not exist");
					Map<String, Expr> insideLocalMap = new HashMap<String, Expr>();
					localMap.put(functionDefinition, insideLocalMap);

				}
				//
				if(type instanceof IntegerType) {
					leftExpr = ictx.mkIntConst(newName);
					this.substitute.put(newName, oldName); 
					this.substituteSort.put(newName, ictx.mkIntSort());
				} if (type instanceof LongType) {
					leftExpr = this.ictx.mkIntConst(newName);
					this.substitute.put(newName, oldName);
					this.substituteSort.put(newName, this.ictx.mkIntSort());
				}
				// fix here
//				localMap.put(oldName, leftExpr);
				localMap.get(functionDefinition).put(oldName, leftExpr);
				return leftExpr;
			} else 
				// fix here
				//return localMap.get(oldName);
				return localMap.get(functionDefinition).get(oldName);
		}
		if(value instanceof BinopExpr) {
			return this.convertBoolExpr((BinopExpr) value, edge, nodeIndex);
		}
		if(value instanceof Constant) {
			Constant constant = (Constant) value;
			if(constant instanceof IntConstant) {
				IntConstant intConstant = (IntConstant) constant;
				int intValue = intConstant.value;
				Expr exprValue = ictx.mkInt(intValue);
				return exprValue;
			}
			if(constant instanceof LongConstant) {
				LongConstant LongConstant = (LongConstant) constant;
				long longValue = LongConstant.value;
				Expr exprValue = ictx.mkInt(longValue);
				return exprValue;

			}
			
		}
		if(value instanceof CastExpr) {
			CastExpr castExpr = (CastExpr) value;
			Value uncasted = castExpr.getOp();
			return this.convertValue(uncasted, assignLeft, edge, nodeIndex);  
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
				LogUtils.debugln("valuePair=" + valuePair);
				Unit unitPair = pair.getUnit();
				LogUtils.debugln("unitPair=" + unitPair);
				
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
			if(resultExpr == null)
				resultExpr = this.ictx.mkInt(0);
			LogUtils.debugln("resultExpr=" + resultExpr);
			return resultExpr;
		}
		if(value instanceof InvokeExpr) {
			if(edge.isFunctionCall()) {
				LogUtils.warningln("****" + value);
				
			}
		}
		if(value instanceof ArrayRef) {
			ArrayRef arrayRef = (ArrayRef) value;
			return arrayHandler.z3ArrayRef(arrayRef, this, edge);

		}
		if(UnitController.isArraysEqualsInvoke(value)) {
			Expr expr = arrayHandler.z3ArraysEqual(value, this, edge);
			return expr;
		}
		if(value instanceof InstanceFieldRef) {
			InstanceFieldRef lclStmt = (InstanceFieldRef) value;
			Expr result = this.z3ObjectField(lclStmt, assignLeft, edge);
			return result;
		}


		LogUtils.fatalln("returning null");
		LogUtils.fatalln("Vertex=" + edge.getSource() + "---Edge=" + edge);
		LogUtils.fatalln("Z3ScriptHandler.convertPrimitiveValue");
		return null;
	}

	private Expr convertRefLikeValue(Value value, boolean assignLeft, Edge edge, int nodeIndex) {
		LogUtils.debugln("Z3ScriptHandler.convertRefLikeValue=" + edge);
		LogUtils.detailln("type  fo the value is " + value.getClass().getName());
 
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
				LogUtils.debugln("valuePair=" + valuePair);
				Unit unitPair = pair.getUnit();
				LogUtils.debugln("unitPair=" + unitPair);
				
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
			if(resultExpr == null)
				resultExpr = this.ictx.mkInt(0);
			LogUtils.debugln("resultExpr=" + resultExpr);
			return resultExpr;
		}
		if(value instanceof Local) {
			Type type = value.getType();		
			Local local = (Local) value;
			if(type instanceof RefType) {
				return createZ3Object(local, assignLeft,  edge);	
			}
			if(type instanceof ArrayType) {
				Expr result = this.arrayHandler.z3Local(local, assignLeft, nodeIndex, this, edge); 
				return result;
			}
		}
		if(value instanceof AnyNewExpr) {
			Expr result = convertAnyNewExpr((AnyNewExpr) value, edge);	
			return result;
		}
		if(value instanceof StringConstant) {
			Expr result = Z3StringHandler.z3NewString(this);
			return result;
		}
		if(value instanceof ArrayRef) {
			ArrayRef arrayRef = (ArrayRef) value;
			return arrayHandler.z3ArrayRef(arrayRef, this, edge);
		}
		if(value instanceof InstanceFieldRef) {
			InstanceFieldRef localStmt = (InstanceFieldRef) value;
			return this.z3InstanceFieldRef(localStmt, assignLeft, edge);
		}
		if(value instanceof CastExpr) {
			CastExpr castExpr = (CastExpr) value;
			Value uncasted = castExpr.getOp();
			return this.convertValue(uncasted, assignLeft, edge, nodeIndex);  
		}	       
		if(value instanceof StaticFieldRef) {
			StaticFieldRef sfRef = (StaticFieldRef) value;
			SootField field = sfRef.getField();
			String name = field.getName();
			Local newLocal = new JimpleLocal(name, field.getType());
			return objFieldHandler.handleStaticFieldRef(newLocal, assignLeft, this);			
		}
		if(value instanceof NullConstant) {
			return this.ictx.mkInt(0);
		}
			
		LogUtils.fatalln("FATAL: Conversion cannot be done");
		LogUtils.fatalln("FATAL: Unit : " + edge.getUnit() + " - Value : " + value);
		LogUtils.fatalln("Z3ScriptHandler.convertRefLikeValue");
		return null;
	}

	private Expr z3InstanceFieldRef(InstanceFieldRef v, boolean IfAssignLeft, Edge edge) {
		Value base = v.getBase();
		SootField Field = v.getField();
		Expr baseZ3 = this.convertValue(base, false, edge, 0);
		String FieldName = Field.toString();
		if (!this.global.containsKey(FieldName)) {
			Sort newArraySort = ictx.mkArraySort(ictx.getIntSort(), ictx.getIntSort());
			String arrayName = this.getGlobalName(FieldName);
			Expr newArray = ictx.mkConst(arrayName,newArraySort);
			this.global.put(FieldName, newArray);
		}
		if (IfAssignLeft) {
			return baseZ3;
		} else {
			ArrayExpr oldArray = (ArrayExpr) this.global.get(FieldName);
			Expr result = ictx.mkSelect(oldArray, baseZ3);
			return result;
		}
	}

	private Expr createZ3Object(Local v,  boolean IfAssignLeft, Edge e) {
		LogUtils.debugln("createZ3Object");
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
			String arrayName = this.getGlobalName(TypeName);
			Expr newArray = ictx.mkConst(arrayName, newArraySort);
			global.put(TypeName, newArray); //theCoverter.updateGlobal(TypeName, newArray);
			substitute.put(TypeName, arrayName);
			NewSort s = new NewSort(newSort, ictx);
			sortId.put(TypeName, s);
		}
		if (IfAssignLeft) {
			String valueName = v.getName() + e.getProgramTree().getProgramDefinition();
			Expr a = ictx.mkConst(valueName, newSortMap.get(TypeName));
			NewSort s2 = sortId.get(TypeName);
			if (s2.ifHasExpr(a)) {
				return a;
			} else {
				s2.creatNewOject(a);
				return a;
			}
		} else {
			ArrayExpr oldArray = (ArrayExpr) global.get(TypeName);
			NewSort s2 = sortId.get(TypeName);
			String valueName = v.getName() + e.getProgramTree().getProgramDefinition(); 
			Expr a = ictx.mkConst(valueName, newSort);// newSortMap.get(TypeName));
			Expr result = this.ictx.mkSelect(oldArray, s2.getId(a));
			//LogUtils.infoln("result=" + result);
			return result;
		}
	}

	public String getGlobalName(String name) {
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

	private BoolExpr convertAssignStmt(Expr rightZ3, Expr leftZ3, Type leftType, Value left, int distance) {
		LogUtils.debugln(leftZ3 + "=" + rightZ3);
		if ((leftType instanceof PrimType) && (left instanceof Local)) {
			BoolExpr leftEqRight = ictx.mkEq(leftZ3, rightZ3);
			return leftEqRight;
		}
		if ((leftType instanceof ArrayType) && (left instanceof Local)) {
			String typeName = leftType.toString();
			String virtualName = typeName;
			String newName = virtualName + this.getNameSuffix();

			ArrayExpr latestArray = (ArrayExpr) this.localMap.get(virtualName);
			LogUtils.debugln("latestArray="+latestArray);
			ArrayExpr newArray = (ArrayExpr) this.ictx.mkConst(newName, latestArray.getSort());
			LogUtils.debugln("newArray=" + newArray);
			this.substitute.put(newName, virtualName);
			this.substituteSort.put(newName, newArray.getSort());
			// fix here
//			this.localMap.put(virtualName, newArray);
			this.localMap.get(currentEdge.getProgramTree().getProgramDefinition()).put(virtualName, newArray);

			String sortName = typeName + this.getArraySortSuffix();
			NewSort s = sortId.get(sortName);

			Expr afterStore = ictx.mkStore((ArrayExpr) latestArray, s.getId(leftZ3), rightZ3);
			LogUtils.debugln("afterStore="+afterStore);
			BoolExpr newArrayEqOldArray = ictx.mkEq(newArray, afterStore);
			return newArrayEqOldArray;
		}
		if (left instanceof ArrayRef) {
			ArrayRef leftRef = (ArrayRef) left;
			BoolExpr result = arrayHandler.updateArrayRef(leftRef, this, rightZ3, currentEdge);
			return result;
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
		
			LogUtils.debugln("afterStore=" + afterStore);
			BoolExpr newArrayEqOldArray = ictx.mkEq(newArray, afterStore);
			return newArrayEqOldArray;
		}	
	}

	private Expr convertAnyNewExpr(AnyNewExpr ane, Edge e) {
		LogUtils.debugln("Z3ScriptHandler.convertAnyNewExpr");
		if(ane instanceof NewExpr) return convertNewExpr((NewExpr)ane, e);
		if(ane instanceof NewArrayExpr) return arrayHandler.convertNewArrayExpr((NewArrayExpr)ane, e, this);
		if(ane instanceof NewMultiArrayExpr) return arrayHandler.convertNewMultiArrayExpr((NewMultiArrayExpr)ane, e, this);
		return null;
	}
	
	private Expr convertNewExpr(NewExpr ne, Edge e) {
		Type t = ne.getType();
		String typeName = t.toString();
		if(sortId.containsKey(typeName)) {
			NewSort s = sortId.get(typeName);
			return s.getNewObject();
		} else {
			Sort newSort = null;
			if(this.newSortMap.containsKey(typeName)) {
				newSort = this.newSortMap.get(typeName);
			} else {
				newSort = this.ictx.mkUninterpretedSort(typeName);
				newSortMap.put(typeName, newSort);
			}
			NewSort ns = new NewSort(newSort, this.ictx);
			sortId.put(typeName, ns);
			return ns.getNewObject();
		}	
	}

	private String getArrayName(Value leftOp) {
		Type t = leftOp.getType();
		if(leftOp instanceof Local) return t.toString();
		else if (leftOp instanceof InstanceFieldRef) {
			InstanceFieldRef ref = (InstanceFieldRef) leftOp;
			SootField field = ref.getField();
			return field.toString();
		}

		// RefHelper.getRrayname
		throw new RuntimeException();
	}

	private String getNameSuffix(Edge e) {
		return "_" + e.getProgramTree().getProgramDefinition() + "_" + e.getSource().getDistance();
	}

	protected String getNameSuffix() {
		return "_" + currentEdge.getProgramTree().getProgramDefinition() + "_" + currentEdge.getSource().getDistance();
	}

	protected String getArrayNameSuffix() {
		return "_" + currentEdge.getProgramTree().getProgramDefinition(); 
	}

	protected String getArraySortSuffix() {
		return "_arraySort";
	}

	protected int getRealArraySize(String name) {
		if(this.realArraySize.containsKey(name)) {
			int size = this.realArraySize.get(name);
			this.realArraySize.put(name, ++size);
			return size;
		} else {
			int size = 1;
			this.realArraySize.put(name, size);
			return size;
		}

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
		if(expr instanceof SubExpr) {
			SubExpr subExpr = (SubExpr) expr;
			Value op1Value = subExpr.getOp1();
			Value op2Value = subExpr.getOp2();

			Expr op1Expr = convertValue(op1Value, false, edge, nodeIndex);
			Expr op2Expr = convertValue(op2Value, false, edge, nodeIndex);
			
			return this.ictx.mkSub((ArithExpr) op1Expr, (ArithExpr) op2Expr);	
		}
		if(expr instanceof MulExpr) {
			MulExpr mullExpr = (MulExpr) expr;
			Value op1Value = mullExpr.getOp1();
			Value op2Value = mullExpr.getOp2();

			Expr op1Expr = convertValue(op1Value, false, edge, nodeIndex);
			Expr op2Expr = convertValue(op2Value, false, edge, nodeIndex);

			return this.ictx.mkMul((ArithExpr) op1Expr, (ArithExpr) op2Expr);	

		}
		if(expr instanceof DivExpr) {
			DivExpr divExpr = (DivExpr) expr;
			Value op1Value = divExpr.getOp1();
			Value op2Value = divExpr.getOp2();

			Expr op1Expr = convertValue(op1Value, false, edge, nodeIndex);
			Expr op2Expr = convertValue(op2Value, false, edge, nodeIndex);

			return this.ictx.mkDiv((ArithExpr) op1Expr, (ArithExpr) op2Expr);	

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
		if(expr instanceof CmpExpr) {
			CmpExpr cmpExpr = (CmpExpr) expr;
			Value op1Value = cmpExpr.getOp1();
			Value op2Value = cmpExpr.getOp2();

			Expr op1Expr = this.convertValue(op1Value, false, edge, edge.getSource().getDistance());
			Expr op2Expr = this.convertValue(op2Value, false, edge, edge.getSource().getDistance());

			return this.ictx.mkSub((ArithExpr) op1Expr, (ArithExpr) op2Expr);
		}
		
		

		LogUtils.fatalln("type  fo the " + expr + " is " + expr.getClass().getName());
		LogUtils.fatalln("Z3ScriptHandler.convertBoolExpr returns null for " + expr);
		return null;
	}
 
	public Expr z3ObjectField(InstanceFieldRef ref, boolean assignLeft, Edge edge) {
		LogUtils.debugln("z3ObjectField");
		Value base = ref.getBase();
		SootField field = ref.getField();
		Expr baseZ3 = this.convertValue(base, false, edge, edge.getSource().getDistance());
		String fieldName = field.toString();
		if(!this.global.containsKey(fieldName)) {
			Sort newArraySort = this.ictx.mkArraySort(this.ictx.getIntSort(), this.ictx.getIntSort());
			String arrayName = this.getGlobalName(fieldName);
			Expr newArray = this.ictx.mkConst(arrayName, newArraySort);
			this.global.put(fieldName, newArray);
		}
		if(assignLeft) {
			return baseZ3;
		} else {
			ArrayExpr oldArray = (ArrayExpr) this.global.get(fieldName);
			Expr result = this.ictx.mkSelect(oldArray, baseZ3);
			return result;
		}
	}
	
	public InterpolationContext getIctx() { return this.ictx; }
	public Map<String, Expr> getGlobal() { return this.global; }
	public Map<String, String> getSubstitute() { return this.substitute; }
	public Map<String, Sort> getSubstituteSort() { return this.substituteSort; }
	public Map<String, Integer> getArrayNameMap() { return this.arrayNameMap; }
	// fix here
	public Map<String, Map<String, Expr>> getLocalMap() { 
		//return this.localMap; 
		return this.localMap;	
	}
	public Map<String, NewSort> getSortId() { return this.sortId; }
	public Map<String, Sort> getNewSortMap() { return this.newSortMap; }
	public Map<String, Integer> getMaxArraySize() { return this.maxArraySize; }

}
