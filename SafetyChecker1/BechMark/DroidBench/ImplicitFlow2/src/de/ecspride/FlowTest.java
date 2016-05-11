package de.ecspride;

import toy_benchmark.ErrorFunction;

public class FlowTest {
	public void caller()
	{
		ImplicitFlow2 if2 = new ImplicitFlow2();
		if2.checkPassword(null);
		ErrorFunction.Error();
	}
}
