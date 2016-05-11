package infoFlow;

import java.util.Comparator;

public class NodeComparator implements Comparator<Node> {

	@Override
	public int compare(Node n1, Node n2) {
		int n1Depth = n1.getDepth();
		int n2Depth = n2.getDepth();
		int n1Shortest = n1.getShortest();
		int n2Shortest = n2.getShortest();
		int n1path = n1.getPath();
		int n2path = n2.getPath();
		if (n1Depth < n2Depth) {
			return -1;
		}
		if (n1Depth > n2Depth) {
			return 1;
		} else {
			if (n1Shortest < n2Shortest) {
				return -1;
			}
			if (n1Shortest > n2Shortest) {
				return 1;
			} else {
				if (n1path < n2path) {
					return -1;
				}
				if (n1path > n2path) {
					return 1;
				}
			}
		}
		return 0;
	}

}
