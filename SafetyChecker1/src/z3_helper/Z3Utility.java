package z3_helper;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.InterpolationContext;
import com.microsoft.z3.Solver;
import com.microsoft.z3.Status;

public class Z3Utility {
	// Check if f1 entails f2.Since if f1 entails f2, it means that all T
	// satisfy f1 that
	// satisfy f2 ,which means no T satisfy f1 and not satisfy f2. In other
	// words,
	// f1 and ~f2 is unsatisfiable.

	public static boolean checkEntail(BoolExpr f1, BoolExpr f2,
			InterpolationContext ictx) {
		BoolExpr notF2 = ictx.mkNot(f2);
		BoolExpr f1EntailF2 = ictx.mkAnd(f1, notF2);
		Solver s = ictx.mkSolver();
		s.reset();
		s.add(f1EntailF2);
		Status result = s.check();
		if (result == Status.UNSATISFIABLE) {
			return true;
		} else {
			return false;
		}
	}

	// pairing function is a bijective function that map n*n -> n
	public static int pairingFunction(int a, int b) {
		int k1 = 0;
		int k2 = 0;
		if (a < b) {
			k1 = b;
			k2 = a;
		} else {
			k1 = a;
			k2 = b;
		}
		int sum1 = k1 + k2;
		int sum2 = k1 + k2 + 1;
		if ((sum1 % 2) == 0) {
			sum1 = sum1 / 2;
		} else {
			sum2 = sum2 / 2;
		}
		int result = sum1 * sum2 + k2;
		return result;
	}
}
