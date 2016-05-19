package safetyChecker;

import java.io.File;
//import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class SafetyInitializerW {
	public static void main(String args[]) {
		//String dirPath = "/Users/ami/Desktop/";
		//File dir = new File(dirPath);
		
		//final String[] okFileExtensions = 
		//	    new String[] {"class"};
			 
		Scanner scanner = new Scanner(System.in);
		System.out.print("Enter a directory name: ");
		System.out.flush();
		String filename = scanner.nextLine();
	
		File file = new File(filename);
		//for (String extension : okFileExtensions) {
		//if (file.getName().endsWith("class")) {
		String[] files = file.list();
		for (String aFile : files) {
			if (aFile.endsWith("class"))
				System.out.println(aFile);
	}
		
	
		
	}
}
