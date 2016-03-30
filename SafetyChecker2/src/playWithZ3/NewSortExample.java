package playWithZ3;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Constructor;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Solver;
import com.microsoft.z3.Sort;
import com.microsoft.z3.Status;

public class NewSortExample {
	public static void main(String[] args) {
		InterpolationContext ictx= new InterpolationContext();
		Constructor cons[] = new Constructor[0];
		Sort newSort=ictx.mkDatatypeSort("Node1",cons );
		
		
	}
}
