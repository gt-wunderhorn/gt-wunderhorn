package dotty;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;

import soot.Unit;
import soot.toolkits.graph.ExceptionalUnitGraph;

import infoFlow.Node;

public class CfgConverter {

	private static BufferedWriter getBufferedWriter(String fileName) throws IOException {
			File file = new File(fileName);
			BufferedWriter writer = new BufferedWriter(new FileWriter(file));
			return writer;
	}

	public static void printAllPaths(Queue<Node> queue, String fileName) {

	}

	public static void printAllPaths2(ExceptionalUnitGraph cfg) {
	}
	
	private static void printSinglePath(Node v, BufferedWriter writer) throws IOException {
	}
}

