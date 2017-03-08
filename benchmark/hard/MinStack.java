public class MinStack {

    private class StackNode {
        private int data;
        private int min;
        private StackNode next;
        
        public StackNode(int data, int min) {
            this.data = data;
            this.min = min;
        }
    }  

    private StackNode top;

    /** initialize your data structure here. */
    public MinStack() {}
    
    public void push(int x) {
        if (top == null) {
            StackNode newTop = new StackNode(x, x);
            top = newTop;
        } else {
            StackNode newTop = new StackNode(x, Math.min(x, top.min));
            newTop.next = top;
            top = newTop;
        }
    }
    
    public void pop() {
        if (top == null) {
            return;
        } else {
            top = top.next;
        }
    }
    
    public int top() {
        return top.data;
    }
    
    public int getMin() {
        return top.min;
    }
}
