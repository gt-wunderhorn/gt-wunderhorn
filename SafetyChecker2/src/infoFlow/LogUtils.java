package infoFlow;

public class LogUtils {

	private static final int DETAIL = 0; 
	private static final int DEBUG = 1;
	private static final int INFO = 2;
	private static final int WARNING = 3;
	private static final int FATAL = 4;

	private static int currentLevel = DEBUG; 

	private static void print(int printLevel, Object... objects) {
		if (currentLevel <= printLevel)
			for (Object object : objects)
				System.out.print(object);
	}

	private static void println(int printLevel,Object... objects) {
		if (currentLevel <= printLevel)
			for (Object object : objects)
				System.out.println(object);
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
}
