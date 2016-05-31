package infoFlow;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

public class NewSort {
	private Sort Type;
	private int currentId;
	private int ObejctId;
	private Map<Expr, IntExpr> index;
	private Set<IntExpr> realObject;
	private InterpolationContext iCtx;
	private int Path;

	public NewSort(Sort Type, InterpolationContext ictx) {
		this.Type = Type;
		this.currentId = 1;
		this.ObejctId = 1;
		this.index = new HashMap<Expr, IntExpr>();
		this.realObject = new HashSet<IntExpr>();
		this.iCtx = ictx;
	}

	// here for the left assign
	public void creatNewOject(Expr name) {
		Integer index = this.currentId;
		IntExpr iExpr = this.iCtx.mkInt(index);
		this.index.put(name, iExpr);
		this.currentId++;
		LogUtils.debugln("NewSort.creatNewOject**name=" + name + "--iExpr" + iExpr + "--currentId-" + currentId);
	}

	public IntExpr getId(Expr name) {
		IntExpr index = this.index.get(name);
		LogUtils.debugln("NewSort.getID**name=" + name + "--index=" + index);
		return index;
	}

	public boolean ifHasExpr(Expr name) {
		LogUtils.debugln("NewSort.ifHasExpr name=" + name);
		return this.index.containsKey(name);
	}

	// here for the right assign
	public Expr getNewObject() {
		int index = this.ObejctId;
		IntExpr newObject = this.iCtx.mkInt(index);
		this.ObejctId++;
		LogUtils.debugln("NewSort.getNewObject index=" + index + "-- ObejctId=" + ObejctId); 
		return newObject;
	}
}
