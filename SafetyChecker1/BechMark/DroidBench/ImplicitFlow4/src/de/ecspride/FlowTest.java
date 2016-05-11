package de.ecspride;

import toy_benchmark.ErrorFunction;

public class FlowTest {
	public void caller()
	{
		ImplicitFlow4 if4 = new ImplicitFlow4();
		if4.checkUsernamePassword(null);
		ErrorFunction.Error();
	}
}
