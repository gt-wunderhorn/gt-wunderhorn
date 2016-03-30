package z3_helper;

import infoFlow.Node;

import java.util.ArrayList;
import java.util.Map;

import com.microsoft.z3.Expr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Sort;

public class ValueHelper {
	//  path number
		private int pathNumber;
	//  path
		private ArrayList<Node> path;
		private InterpolationContext iCtx;
	// offer support for all substitute
		private Map<String, String> renameSet;
	// when we use right side, we need to know the latest rename constant
		private Map<String, Expr> latestSet;
	// used for phi function
		private ArrayList<String> latestVariable;
	// used for policy assignment
		private ArrayList<Expr> assignInput;
	// used for generate policy assignment
		private ArrayList<Expr> localVariable;
	// used for new sort index map to an integer
		private Map<String, NewSort> sortId;
	// usde for get sort in z3
		private Map<String, Sort> newSort;
		
	public ValueHelper(InterpolationContext iCtx,int pathNumber,ArrayList<Node> path){
		this.pathNumber=pathNumber;
		this.iCtx=iCtx;
		
	}
}
