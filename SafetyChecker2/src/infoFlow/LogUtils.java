package infoFlow;

public class LogUtils {

	// Logging levels
	private static final int DETAIL = 0; 
	private static final int DEBUG = 1;
	private static final int INFO = 2;
	private static final int WARNING = 3;
	private static final int FATAL = 4;
	
	private static final int RESET = 100;
	private static final int INFEASIBLE = 101;
	private static final int FEASIBLE = 102;

	// flag for the printing level
	// lowever levels are not printing
	private static int currentLevel = INFO; 

	// for reset
	public static final String ANSI_RESET = "\u001B[0m";
	// for fatal outputs -- RED
	public static final String ANSI_RED = "\u001B[31m";
	// for infeasible result outputs -- GREEN
	public static final String ANSI_GREEN = "\u001B[32m";
	// for warning outputs -- YELLOW
	public static final String ANSI_YELLOW = "\u001B[33m";
	// for debug outputs -- BLUE
	public static final String ANSI_BLUE = "\u001B[34m";
	// for feasible result outputs -- PURPLE
	public static final String ANSI_PURPLE = "\u001B[35m";
	// for info outputs -- CYAN
	public static final String ANSI_CYAN = "\u001B[36m";

	public static String getOutputColor(int printLevel) {
		String color = ANSI_RESET; 
		switch (printLevel) {
			case DEBUG : color = ANSI_BLUE; break;
			case INFO : color = ANSI_CYAN; break;
			case WARNING : color = ANSI_YELLOW; break;
			case FATAL : color = ANSI_RED; break; 
			case RESET : color = ANSI_RESET; break;
			case INFEASIBLE : color = ANSI_GREEN; break;
			case FEASIBLE : color = ANSI_PURPLE; break;
		}	
		return color;
	}

	private static void print(int printLevel, Object... objects) {
		if (currentLevel <= printLevel)
			for (Object object : objects)
				System.out.print(getOutputColor(printLevel) + object + getOutputColor(RESET));
	}

	private static void println(int printLevel,Object... objects) {
		if (currentLevel <= printLevel)
			for (Object object : objects)
				System.out.println(getOutputColor(printLevel) + object + getOutputColor(RESET));
	}

	public static void debug(Object... objects) {
		print(DEBUG, objects);
	}

	public static void debugln(Object... objects) {
		println(DEBUG, objects);
	}

	public static void info(Object... objects) {
		print(INFO, objects);
	}

	public static void infoln(Object... objects) {
		println(INFO, objects);
	}

	public static void fatal(Object... objects) {
		print(FATAL, objects);
	}

	public static void fatalln(Object... objects) {
		println(FATAL, objects);
	}
	
	public static void detail(Object... objects) {
		print(DETAIL, objects);
	}

	public static void detailln(Object... objects) {
		println(DETAIL, objects);
	}

	public static void warning(Object... objects) {
		print(WARNING, objects);
	}

	public static void warningln(Object... objects) {
		println(WARNING, objects);
	}
	
	public static void nextLine(int printLevel) {
		if (currentLevel <= printLevel)
			System.out.println();
	}

	public static void printResult(String functionName, boolean result) {
		if(result) { 
			println(FEASIBLE, "********************\n " + functionName);
			println(FEASIBLE, "Error path is feasible.");
			println(FEASIBLE, "********************");
		} else {
			println(INFEASIBLE, "********************\n " + functionName);
			println(INFEASIBLE, "Error path is not feasible.");
			println(INFEASIBLE, "********************");
		}
	}

}
