
public class LinkedList {
	class Node {
		Node next;
		int value;

		public Node(int a) {
			this.value = a;
		}
	}

	private Node head;
	private Node tail;

	public LinkedList() {
		head = new Node(0);
		tail = head;
	}

	public void add(int n) {
		Node a = new Node(n);
		tail.next = a;
		tail = a;
	}

	public int get(int size) {
		Node theNode = this.head;
		size++;
		int count=0;
		while(count<size) {
			theNode = theNode.next;
			count++;
		}
		return theNode.value;
	}
}
