package safetyChecker.utilities;

public @interface ConsraintAnnotation {
	boolean constraint() default false;
}
