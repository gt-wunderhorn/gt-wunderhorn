package infoFlow;

import java.util.Comparator;

public class NodeComparator implements Comparator<Node> {

	@Override
	public int compare(Vertex v1, Vertex v2) {
		int v1Depth = v1.getDepth();
		int v2Depth = v2.getDepth();
		int v1Shortest = v1.getShortest();
		int v2Shortest = v2.getShortest();
		int v1path = v1.getPath();
		int n2path = v2.getPath();
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
