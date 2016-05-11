package z3_helper;

import soot.Local;
import soot.SootField;
import soot.Type;
import soot.Value;
import soot.jimple.InstanceFieldRef;
import soot.jimple.StaticFieldRef;

public class RefHelper {
	static public String getArrayName(Value leftOp){
		Type t=leftOp.getType();
		if (leftOp instanceof Local){
			return t.toString();
		}
		else{
			if (leftOp instanceof InstanceFieldRef){
				InstanceFieldRef iField=(InstanceFieldRef) leftOp;
				SootField field=iField.getField();
				return field.toString();
			}
		}
		if (leftOp instanceof StaticFieldRef){
			Type t2=leftOp.getType();
			return t2.toString();
		}
		return null;
	}
}
