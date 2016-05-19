package z3_helper;

import infoFlow.Forest;
import infoFlow.Node;
import infoFlow.Tree;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import soot.ArrayType;
import soot.DoubleType;
import soot.IntType;
import soot.IntegerType;
import soot.Local;
import soot.PrimType;
import soot.RefLikeType;
import soot.RefType;
import soot.SootField;
import soot.Type;
import soot.Unit;
import soot.Value;
import soot.jimple.AddExpr;
import soot.jimple.AndExpr;
import soot.jimple.AnyNewExpr;
import soot.jimple.ArrayRef;
import soot.jimple.AssignStmt;
import soot.jimple.BinopExpr;
import soot.jimple.CastExpr;
import soot.jimple.CmpExpr;
import soot.jimple.CmpgExpr;
import soot.jimple.CmplExpr;
import soot.jimple.ConditionExpr;
import soot.jimple.Constant;
import soot.jimple.DivExpr;
import soot.jimple.DoubleConstant;
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
import soot.jimple.LtExpr;
import soot.jimple.MulExpr;
import soot.jimple.NeExpr;
import soot.jimple.NegExpr;
import soot.jimple.NewExpr;
import soot.jimple.NullConstant;
import soot.jimple.OrExpr;
import soot.jimple.RealConstant;
import soot.jimple.RemExpr;
import soot.jimple.ReturnStmt;
import soot.jimple.ReturnVoidStmt;
import soot.jimple.ShlExpr;
import soot.jimple.ShrExpr;
import soot.jimple.StaticFieldRef;
import soot.jimple.StringConstant;
import soot.jimple.SubExpr;
import soot.jimple.UshrExpr;
import soot.jimple.XorExpr;
import soot.jimple.internal.JNewExpr;
import soot.jimple.internal.JimpleLocal;
import soot.shimple.PhiExpr;
import soot.toolkits.scalar.ValueUnitPair;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.ArrayExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

public class PathHelper {
	private static final boolean AssignLeft = false;
	// pathCoverter
	private PathCoverter theCoverter;
	private Map<String, Tree> subFunctionTree;
	private Map<String, Expr> latestLocal;
	private int level;
	private ArrayList<Node> path;
	private InterpolationContext iCtx;
	private Map<String, Integer> size;
	private Forest theForest;

	public PathHelper(PathCoverter theCoverter,
			Map<String, Tree> subFunctionTree, ArrayList<Node> path, Forest f1) {
		this.theCoverter = theCoverter;
		this.subFunctionTree = subFunctionTree;
		this.latestLocal = new HashMap<String, Expr>();
		this.level = theCoverter.getLevel();
		this.path = path;
		this.iCtx = theCoverter.getIctx();
		this.size = new HashMap<String, Integer>();
		this.theForest = f1;
	}

	public ArrayList<BoolExpr> ConvertToZ3() {
		Map<String, Expr> latestLocal = new HashMap<String, Expr>();
		ArrayList<BoolExpr> result = new ArrayList<BoolExpr>();
		this.theCoverter.storeResult(result);
		for (int i = 0; i < path.size(); i++) {
			Node n = path.get(i);
			if(n.getIfArryCopy()){
				InvokeStmt iStmt = (InvokeStmt) n.getStmt();
				BoolExpr constrains=SpecialInvoke.handleArrayCopy(iStmt, this, theCoverter);
				result.add(constrains);
				continue;
			}
			if (n.getIfNew()) {
				Tree loadFunction = this.theForest.getLoadFunction();
				ArrayList<Node> nextPath = loadFunction.getPath(0);
				PathHelper nextLevel = new PathHelper(this.theCoverter,
						this.subFunctionTree, nextPath, this.theForest);
				nextLevel.ConvertToZ3();
			}
			if (n.getIfSink()) {
				InvokeStmt iStmt = (InvokeStmt) n.getStmt();
				BoolExpr constrains = PolicyHelper.storeSink(iStmt, this,
						theCoverter);
				result.add(constrains);
				continue;
			}
			if (n.ifError()) {
				InvokeStmt iStmt = (InvokeStmt) n.getStmt();
				PolicyHelper.storeResult(iStmt, this, theCoverter);
				continue;
			}
			if (n.ifObject()) {
				result.add(this.iCtx.mkTrue());
				continue;
			}
			Unit u = n.getStmt();
			// current support IfStmt, GoToStmt, AssignStmt
			if (u instanceof IfStmt) {
				IfStmt current = (IfStmt) u;
				Value v = current.getCondition();
				BoolExpr condition = (BoolExpr) this.CovertValue(v, false, i);
				// if stmt is last stmt
				if (i + 1 == path.size()) {
					BoolExpr notCondition = theCoverter.getIctx().mkNot(
							(BoolExpr) condition);
					result.add(notCondition);
				} else {
					Node nextNode = path.get(i + 1);
					Unit nextStmt = nextNode.getStmt();
					Unit targetStmt = current.getTarget();
					if (targetStmt.equals(nextStmt)) {
						result.add(condition);
					} else {
						BoolExpr notCondition = theCoverter.getIctx().mkNot(
								(BoolExpr) condition);
						result.add(notCondition);
					}
				}
			}
			if (u instanceof GotoStmt) {
				BoolExpr alwaysTrue = theCoverter.getIctx().mkTrue();
				result.add(alwaysTrue);
			}
			if (u instanceof AssignStmt) {

				AssignStmt aStmt = (AssignStmt) u;
				Value left = aStmt.getLeftOp();
				Value right = aStmt.getRightOp();
				if (n.getIfNonsense()) {
					if (left.getType() instanceof RefType) {
						right = new JNewExpr((RefType) left.getType());
					}
				}
				if (n.ifDummy()) {
					InvokeExpr iExpr = (InvokeExpr) right;
					String methodSignature = iExpr.getMethod().getSignature();
					Tree nextFunction = this.subFunctionTree
							.get(methodSignature);
					Node nextNode = path.get(i + 1);
					int index = nextNode.getPath();
					ArrayList<Node> nextPath = nextFunction.getPath(index);
					PathHelper nextLevel = new PathHelper(this.theCoverter,
							this.subFunctionTree, nextPath, this.theForest);
					result.add(InvokeHelper.CallToZ3(iExpr, this,
							this.theCoverter, this.level, i));
					nextLevel.ConvertToZ3();
				} else {
					/**
					 * if (right instanceof StringConstant) { StringConstant s =
					 * (StringConstant) right; Expr leftZ3 =
					 * this.CovertValue(left, true, i);
					 * System.out.println(leftZ3);
					 * result.add(StringHelper.covertString(s, this.theCoverter,
					 * (ArrayExpr) leftZ3)); continue; }
					 **/

					Type leftType = left.getType();
					Expr rightZ3 = null;
					if (right instanceof InvokeExpr) {
						if (!n.getIfNonsense()) {
							rightZ3 = InvokeHelper.returnToZ3(this.theCoverter);
						} else {
							rightZ3 = this.iCtx.mkIntConst("nonsense"
									+ this.theCoverter
											.getRealArraySize("nonsense"));
						}
					} else {
						rightZ3 = this.CovertValue(right, false, i);
					}
					Expr leftZ3 = this.CovertValue(left, true, i);
					if (n.getIfInput()) {
						rightZ3 = StringHelper.z3NewString(theCoverter, this);
					}
					if (n.getIfNonsenseCompare()) {
						rightZ3 = this.iCtx.mkIntConst("nonsenscompare"
								+ theCoverter
										.getRealArraySize("nonsenscompare"));
					}
					//System.out.println("the stmt is " + n.getStmt());
					BoolExpr halfExpr = CovertAssignStmt(rightZ3, leftZ3,
							leftType, left, i);
					if (n.getIfInput()) {
						BoolExpr f1 = (BoolExpr) SpecialInvoke.addSensitive(
								this.theCoverter, rightZ3);
						// and
						BoolExpr wholeFormula = this.theCoverter.getIctx()
								.mkAnd(halfExpr, f1);
						result.add(wholeFormula);
						continue;
					}
					if (right instanceof StringConstant) {
						StringConstant sConstant = (StringConstant) right;
						BoolExpr f1 = StringHelper.covertString(sConstant,
								theCoverter, rightZ3);
						BoolExpr wholeFormula = this.theCoverter.getIctx()
								.mkAnd(halfExpr, f1);
						result.add(wholeFormula);
					} else {
						if (right instanceof AnyNewExpr) {
							BoolExpr spaceFormula = NewExprHelper.spaceFormula(
									right, theCoverter, this.latestLocal, this,
									rightZ3);
							BoolExpr wholeFormula = this.theCoverter.getIctx()
									.mkAnd(halfExpr, spaceFormula);
							result.add(wholeFormula);
						} else {
							result.add(halfExpr);
						}
					}
				}
			}
			if (u instanceof InvokeStmt) {
				if (n.getIfNonsense()) {
					result.add(this.iCtx.mkTrue());
					continue;
				}
				if (n.ifDummy()) {
					InvokeStmt iStmt = (InvokeStmt) u;
					Node nextNode = path.get(i + 1);
					int index = nextNode.getPath();
					InvokeExpr iExpr = iStmt.getInvokeExpr();
					String methodSignature = iExpr.getMethod().getSignature();
					Tree nextFunction = this.subFunctionTree
							.get(methodSignature);
					ArrayList<Node> nextPath = nextFunction.getPath(index);
					PathHelper nextLevel = new PathHelper(this.theCoverter,
							this.subFunctionTree, nextPath, this.theForest);
					BoolExpr r1 = InvokeHelper.CallToZ3(iExpr, this,
							this.theCoverter, this.level, i);
					result.add(r1);
					nextLevel.ConvertToZ3();
				} else {
					BoolExpr alwaysTrue = this.theCoverter.getIctx().mkTrue();
					InvokeHelper.returnToZ3(this.theCoverter);
					result.add(alwaysTrue);
				}
			}
			if (u instanceof IdentityStmt) {
				IdentityStmt iStmt = (IdentityStmt) u;
				Value left = iStmt.getLeftOp();
				Expr leftZ3 = this.CovertValue(left, true, i);
				if (this.theCoverter.ifParaEmpty()) {
					Type t = left.getType();
					if (t instanceof RefType) {
						RefType rType = (RefType) t;
						NewExpr right = new JNewExpr(rType);
						Expr rightZ3 = this.CovertValue(right, false, 0);
						Type leftType = left.getType();
						BoolExpr expr = CovertAssignStmt(rightZ3, leftZ3,
								leftType, left, i);
						result.add(expr);
					} else {
						result.add(this.iCtx.mkTrue());
					}
				} else {
					Expr rightZ3 = this.theCoverter.popPara();
					Type leftType = left.getType();
					BoolExpr expr = CovertAssignStmt(rightZ3, leftZ3, leftType,
							left, i);
					result.add(expr);
				}

			}
			if (u instanceof ReturnStmt) {
				ReturnStmt rStmt = (ReturnStmt) u;
				BoolExpr returnExpr = InvokeHelper.returnStmt(rStmt, this,
						this.theCoverter, this.level, i);
				result.add(returnExpr);
			}
			if (u instanceof ReturnVoidStmt) {
				InvokeHelper.returnVoid(this.theCoverter);
				BoolExpr alwaystrue = this.theCoverter.getIctx().mkTrue();
				result.add(alwaystrue);
			}
			  System.out.println("------------------------------");
			  System.out.println("statment:"+u+"\n");
			  System.out.println("constrains"+result.get(result.size()-1)+"\n");
		}
		return result;
	}

	public BoolExpr CovertAssignStmt(Expr rightZ3, Expr leftZ3, Type leftType,
			Value left, int i) {
		if ((leftType instanceof PrimType) && (left instanceof Local)) {
			BoolExpr leftEqRight = iCtx.mkEq(leftZ3, rightZ3);
			return leftEqRight;
		}
		if ((leftType instanceof ArrayType) && (left instanceof Local)) {
			String typeName = leftType.toString();
			String virtualName = typeName + "Path"
					+ theCoverter.getPathNumber() + "level" + level;
			String newName = virtualName + "index" + i;
			ArrayExpr latestArray = (ArrayExpr) this.latestLocal
					.get(virtualName);
			ArrayExpr newArray = (ArrayExpr) this.theCoverter.getIctx()
					.mkConst(newName, latestArray.getSort());
			theCoverter.addSubstitute(newName, virtualName);
			theCoverter.updateSubstituteSort(newName, newArray.getSort());
			if (this.latestLocal.containsKey(virtualName)) {
				this.latestLocal.remove(virtualName);
				this.latestLocal.put(virtualName, newArray);
			} else {
				this.latestLocal.put(virtualName, newArray);
			}
			String sortName = typeName + "virtual" + "level" + level;
			NewSort s = this.theCoverter.getSortId().get(sortName);
			Expr afterStore = iCtx.mkStore((ArrayExpr) latestArray,
					s.getId(leftZ3), rightZ3);
			BoolExpr newArrayEqOldArray = iCtx.mkEq(newArray, afterStore);
			return newArrayEqOldArray;
		}
		if (left instanceof ArrayRef) {
			ArrayRef leftV = (ArrayRef) left;
			return ArrayHelper
					.updateArrayRef(leftV, theCoverter, this, rightZ3);
		} else {
			String oldName = RefHelper.getArrayName(left);
			String newName = this.theCoverter.getGlobalName(oldName);
			Expr latestArray = this.theCoverter.getGlobal(oldName);
			Expr newArray = this.iCtx.mkConst(newName, latestArray.getSort());
			this.theCoverter.addSubstitute(newName, oldName);
			this.theCoverter.updateGlobal(oldName, newArray);
			NewSort s = this.theCoverter.getSortId().get(oldName);
			Expr afterStore = null;
			if ((left instanceof FieldRef)
					&& (!(left instanceof StaticFieldRef))) {
				afterStore = iCtx.mkStore((ArrayExpr) latestArray, leftZ3,
						rightZ3);
			} else {
				afterStore = iCtx.mkStore((ArrayExpr) latestArray,
						s.getId(leftZ3), rightZ3);

			}
			BoolExpr newArrayEqOldArray = iCtx.mkEq(newArray, afterStore);
			return newArrayEqOldArray;
		}
	}

	public Expr CovertValue(Value v, boolean IfAssignLeft, int nodeIndex) {
		Type valueType = v.getType();
		if (valueType instanceof PrimType) {
			return CovertPrimitiveValue(v, IfAssignLeft, nodeIndex);
		}
		if (valueType instanceof RefLikeType) {
			return CovertRefLikeValue(v, IfAssignLeft, nodeIndex);
		}
		System.err.println(v + ": this value type is not supported "
				+ v.getType());
		return null;
	}

	private Expr CovertRefLikeValue(Value v, boolean IfAssignLeft, int nodeIndex) {
		if (v instanceof PhiExpr) {
			PhiExpr phiE1 = (PhiExpr) v;
			List<ValueUnitPair> L1Value = phiE1.getArgs();
			Value currentOne = null;
			int closer = Integer.MAX_VALUE;
			for (int i = 0; i < L1Value.size(); i++) {
				ValueUnitPair vUp = L1Value.get(i);
				Value v1 = vUp.getValue();
				Unit u1 = vUp.getUnit();
				int count = 0;
				Node currentNode = this.path.get(nodeIndex);
				//CHANGE ONE LINE
//				Node compareNode = currentNode.predecessor();
				Node compareNode = currentNode.successor();
				Unit compareNodeUnit = compareNode.getStmt();
				//CHANGE ONE LINE
//				while ((!u1.equals(compareNodeUnit)) && (compareNode.predecessor() != null)) {
				while ((!u1.equals(compareNodeUnit)) && (compareNode.successor() != null)) {
					count++;
					//CHANGE ONE LINE
//					compareNode = compareNode.predecessor();
					compareNode = compareNode.successor();
					compareNodeUnit = compareNode.getStmt();
				}
				if (count < closer) {
					currentOne = v1;
					closer = count;
				}
			}
			return this.CovertValue(currentOne, false, nodeIndex);
		}
		if (v instanceof Local) {
			Type valueType = v.getType();
			Local localStmt = (Local) v;
			if (valueType instanceof RefType) {
				String typeName = valueType.toString();
				return ObjectHelper.z3Object(localStmt, IfAssignLeft,
						this.theCoverter, this.level);
			}
			if (valueType instanceof ArrayType) {
				//System.out.println(localStmt);
				return ArrayHelper.z3Local(localStmt, IfAssignLeft,
						this.theCoverter, nodeIndex, this.latestLocal,
						this.level);
			}

		}
		if (v instanceof AnyNewExpr) {
			AnyNewExpr localStmt = (AnyNewExpr) v;
			return NewExprHelper.z3NewExpr(localStmt, this.theCoverter,
					this.latestLocal, this);
		}
		if (v instanceof StringConstant) {
			return StringHelper.z3NewString(this.theCoverter, this);
		}
		if (v instanceof ArrayRef) {
			ArrayRef localStmt = (ArrayRef) v;
			return ArrayHelper.z3Object(localStmt, theCoverter, this,
					this.latestLocal);
		}
		if (v instanceof InstanceFieldRef) {
			InstanceFieldRef localStmt = (InstanceFieldRef) v;
			return FieldHelper.z3Object((InstanceFieldRef) localStmt,
					IfAssignLeft, this.theCoverter, this);
		}
		if (v instanceof CastExpr) {
			CastExpr cExpr = (CastExpr) v;
			Value old = cExpr.getOp();
			return this.CovertValue(old, IfAssignLeft, nodeIndex);
		}
		if (v instanceof StaticFieldRef) {
			StaticFieldRef sFieldRef = (StaticFieldRef) v;
			SootField field = sFieldRef.getField();
			String name = field.getName();
			Local newLocal = new JimpleLocal(name, field.getType());
			return StaticFieldHelper.staticField(newLocal, IfAssignLeft,
					theCoverter, nodeIndex);

		}
		if (v instanceof NullConstant) {
			return this.iCtx.mkInt(0);
		}
		System.err.println("unsupported type" + v.getClass() + "what is v");
		return null;
	}

	private Expr CovertPrimitiveValue(Value v, boolean IfAssignLeft,
			int nodeIndex) {
		if (v instanceof Local) {
			Local left1 = (Local) v;
			String oldName = left1.getName();
			// we need rename here
			if (IfAssignLeft) {
				Type theType=v.getType();
				String newName = oldName
						+ this.theCoverter.getRenameString(this.level)
						+ "index" + nodeIndex;
				Expr leftExpr=null;
				if(theType instanceof IntegerType){
				leftExpr = this.iCtx.mkIntConst(newName);
				theCoverter.addSubstitute(newName, oldName);
				theCoverter.updateSubstituteSort(newName,
						this.iCtx.getIntSort());
				}
				if(theType instanceof DoubleType){
					leftExpr = this.iCtx.mkRealConst(newName);
					theCoverter.addSubstitute(newName, oldName);
					theCoverter.updateSubstituteSort(newName,
							this.iCtx.getRealSort());
				}
				if(leftExpr==null){
					System.err.println("there is an error, that in Local covert\n type is"+theType.toString());
				}
				if (this.latestLocal.containsKey(oldName)) {
					this.latestLocal.remove(oldName);
					this.latestLocal.put(oldName, leftExpr);
				} else {
					this.latestLocal.put(oldName, leftExpr);
				}
				return leftExpr;
			} else {
				return this.latestLocal.get(oldName);
			}
		}
		if (v instanceof BinopExpr) {
			BinopExpr b = (BinopExpr) v;
			return CovertBoolExpr(b, nodeIndex);
		}
		if (v instanceof Constant) {
			Constant c = (Constant) v;
			if (c instanceof IntConstant) {
				IntConstant IntC = (IntConstant) c;
				int value = IntC.value;
				Expr IntValue = this.iCtx.mkInt(value);

				return IntValue;
			} 
			if (c instanceof DoubleConstant){
				DoubleConstant relC=(DoubleConstant) c;
				double value=relC.value;
				Expr realValue=this.iCtx.mkReal(Double.toString(value));
				return realValue;
			}
			else {
				System.err.println("unsupported primitive constant type" + c);
			}
		}
		if (v instanceof NegExpr) {
			NegExpr negE1 = (NegExpr) v;
			Value op1 = negE1.getOp();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			return this.iCtx.mkSub(this.iCtx.mkInt(0), (ArithExpr) op1Expr);
		}
		if (v instanceof PhiExpr) {
			PhiExpr phiE1 = (PhiExpr) v;
			List<ValueUnitPair> L1Value = phiE1.getArgs();
			Value currentOne = null;
			int closer = Integer.MAX_VALUE;
			for (int i = 0; i < L1Value.size(); i++) {
				ValueUnitPair vUp = L1Value.get(i);
				Value v1 = vUp.getValue();
				Unit u1 = vUp.getUnit();
				int count = 0;
				Node currentNode = this.path.get(nodeIndex);
				//CHANGE ONE LINE
//				Node compareNode = currentNode.predecessor();
				Node compareNode = currentNode.successor();
				Unit compareNodeUnit = compareNode.getStmt();
				//CHANGE ONE LINE
//				while ((!u1.equals(compareNodeUnit)) && (compareNode.predecessor() != null)) {
				while ((!u1.equals(compareNodeUnit)) && (compareNode.successor() != null)) {
					count++;
					//CHANGE ONE LINE
//					compareNode = compareNode.predecessor();
					compareNode = compareNode.successor();
					compareNodeUnit = compareNode.getStmt();
				}
				if (count < closer) {
					currentOne = v1;
					closer = count;
				}
			}
			return this.CovertValue(currentOne, false, nodeIndex);
		}
		if (v instanceof AnyNewExpr) {
			AnyNewExpr localStmt = (AnyNewExpr) v;
			return NewExprHelper.z3NewExpr(localStmt, this.theCoverter,
					this.latestLocal, this);
		}
		if (v instanceof ArrayRef) {
			ArrayRef localStmt = (ArrayRef) v;
			return ArrayHelper.z3Object(localStmt, this.theCoverter, this,
					this.latestLocal);
		}
		if (v instanceof InstanceFieldRef) {
			InstanceFieldRef localStmt = (InstanceFieldRef) v;
			return FieldHelper.z3Object((InstanceFieldRef) localStmt,
					IfAssignLeft, this.theCoverter, this);
		}
		System.err.println("unsupported primitive value type" + v);
		return null;
	}

	private Expr CovertBoolExpr(BinopExpr v, int nodeIndex) {
		if (v instanceof AddExpr) {
			AddExpr addE1 = (AddExpr) v;
			Value op1 = addE1.getOp1();
			Value op2 = addE1.getOp2();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			Expr op2Expr = this.CovertValue(op2, false, nodeIndex);
			return this.iCtx.mkAdd((ArithExpr) op1Expr, (ArithExpr) op2Expr);
		}
		if (v instanceof AndExpr) {
			AndExpr andE1 = (AndExpr) v;
			Value op1 = andE1.getOp1();
			Value op2 = andE1.getOp2();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			Expr op2Expr = this.CovertValue(op2, false, nodeIndex);
			return this.iCtx.mkAnd((BoolExpr) op1Expr, (BoolExpr) op2Expr);
		}
		if (v instanceof CmpExpr) {
			// Don't know what's the meaning of CmpExpr, return False now
			System.out.println("CmpExpr is not handle here");
			return this.iCtx.mkFalse();
		}
		if (v instanceof CmpgExpr) {
			// Don't know what's the meaning of CmpgExpr, return False now
			System.out.println("CmpgExpr is not handle here");
			return this.iCtx.mkFalse();
		}
		if (v instanceof CmplExpr) {
			// Don't know what's the meaning of CmplExpr, return False now
			System.out.println("CmplExpr is not handle here");
			return this.iCtx.mkFalse();
		}
		if (v instanceof ConditionExpr) {
			// Don't do anything here, handle this in different subclass
		}
		if (v instanceof DivExpr) {
			// we only consider integer here, we may consider real later
			DivExpr divE1 = (DivExpr) v;
			Value op1 = divE1.getOp1();
			Value op2 = divE1.getOp2();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			Expr op2Expr = this.CovertValue(op2, false, nodeIndex);
			return this.iCtx.mkDiv((ArithExpr) op1Expr, (ArithExpr) op2Expr);
		}
		if (v instanceof EqExpr) {
			EqExpr eqE1 = (EqExpr) v;
			Value op1 = eqE1.getOp1();
			Value op2 = eqE1.getOp2();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			Expr op2Expr = this.CovertValue(op2, false, nodeIndex);
			return this.iCtx.mkEq(op1Expr, op2Expr);
		}
		if (v instanceof GeExpr) {
			GeExpr geE1 = (GeExpr) v;
			Value op1 = geE1.getOp1();
			Value op2 = geE1.getOp2();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			Expr op2Expr = this.CovertValue(op2, false, nodeIndex);
			return this.iCtx.mkGe((ArithExpr) op1Expr, (ArithExpr) op2Expr);
		}
		if (v instanceof GtExpr) {
			GtExpr gtE1 = (GtExpr) v;
			Value op1 = gtE1.getOp1();
			Value op2 = gtE1.getOp2();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			Expr op2Expr = this.CovertValue(op2, false, nodeIndex);
			return this.iCtx.mkGt((ArithExpr) op1Expr, (ArithExpr) op2Expr);
		}
		if (v instanceof LeExpr) {
			LeExpr leE1 = (LeExpr) v;
			Value op1 = leE1.getOp1();
			Value op2 = leE1.getOp2();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			Expr op2Expr = this.CovertValue(op2, false, nodeIndex);
			return this.iCtx.mkLe((ArithExpr) op1Expr, (ArithExpr) op2Expr);
		}
		if (v instanceof LtExpr) {
			LtExpr ltE1 = (LtExpr) v;
			Value op1 = ltE1.getOp1();
			Value op2 = ltE1.getOp2();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			Expr op2Expr = this.CovertValue(op2, false, nodeIndex);
			return this.iCtx.mkLt((ArithExpr) op1Expr, (ArithExpr) op2Expr);
		}
		if (v instanceof MulExpr) {
			MulExpr mulE1 = (MulExpr) v;
			Value op1 = mulE1.getOp1();
			Value op2 = mulE1.getOp2();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			Expr op2Expr = this.CovertValue(op2, false, nodeIndex);
			return this.iCtx.mkMul((ArithExpr) op1Expr, (ArithExpr) op2Expr);
		}
		if (v instanceof NeExpr) {
			NeExpr neE1 = (NeExpr) v;
			Value op1 = neE1.getOp1();
			Value op2 = neE1.getOp2();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			Expr op2Expr = this.CovertValue(op2, false, nodeIndex);
			BoolExpr eqExpr = this.iCtx.mkEq((ArithExpr) op1Expr,
					(ArithExpr) op2Expr);
			return this.iCtx.mkNot(eqExpr);
		}
		if (v instanceof OrExpr) {
			OrExpr orE1 = (OrExpr) v;
			Value op1 = orE1.getOp1();
			Value op2 = orE1.getOp2();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			Expr op2Expr = this.CovertValue(op2, false, nodeIndex);
			return this.iCtx.mkOr((BoolExpr) op1Expr, (BoolExpr) op2Expr);
		}
		if (v instanceof RemExpr) {
			RemExpr remE1 = (RemExpr) v;
			Value op1 = remE1.getOp1();
			Value op2 = remE1.getOp2();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			Expr op2Expr = this.CovertValue(op2, false, nodeIndex);
			return this.iCtx.mkRem((IntExpr) op1Expr, (IntExpr) op2Expr);
		}
		if (v instanceof ShlExpr) {
			// do later
			System.err.println("ShlExpr is unsupported" + v);
		}
		if (v instanceof ShrExpr) {
			// do later
			System.err.println("ShrExpr is unsupported" + v);
		}
		if (v instanceof SubExpr) {
			SubExpr subE1 = (SubExpr) v;
			Value op1 = subE1.getOp1();
			Value op2 = subE1.getOp2();
			Expr op1Expr = this.CovertValue(op1, false, nodeIndex);
			Expr op2Expr = this.CovertValue(op2, false, nodeIndex);
			return this.iCtx.mkSub((ArithExpr) op1Expr, (ArithExpr) op2Expr);
		}
		if (v instanceof UshrExpr) {
			// do later
			System.err.println("UshrExpr is unsupported" + v);
		}
		if (v instanceof XorExpr) {
			// do later
			System.err.println("XorExpr is unsupported" + v);
		}
		System.err.println("this BinopExpr is unsupported" + v);
		return null;
	}

	public Map<String, Expr> getLatestLocal() {
		return this.latestLocal;
	}

	public int getLevel() {
		return this.level;
	}
}
