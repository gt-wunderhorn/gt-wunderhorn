package safetyChecker.utilities;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;

import soot.Body;
import soot.BodyTransformer;
import soot.PackManager;
import soot.SootMethod;
import soot.Transform;
import soot.options.Options;

public class SafetyCheckerGUI extends JPanel implements TreeSelectionListener {

	JPanel mainPanel, selectPanel, constraintPanel, resultPanel, apiPanel;
	JComboBox parameterTypeBox;
	JLabel apiListLabel;
	JScrollPane treeView = new JScrollPane();

	public SafetyCheckerGUI(Container pane) {
		// Create panels.
		// Add various widgets to the sub panels.
		addWidgets();

		// Create the main panel to contain the two sub panels.
		mainPanel = new JPanel();
		// mainPanel.setLayout(new BoxLayout(mainPanel, BorderLayout.));
		mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

		// Add the select and display panels to the main panel.
		pane.add(selectPanel, BorderLayout.PAGE_START);
		pane.add(apiPanel, BorderLayout.LINE_START);
		pane.add(constraintPanel, BorderLayout.CENTER);
		pane.add(resultPanel, BorderLayout.LINE_END);
	}

	private void addWidgets() {
		selectPanel = new JPanel();
		resultPanel = new JPanel();
		apiPanel = new JPanel();
		constraintPanel = new JPanel();

		addWidgets2SelectPanel();
		addWidgets2APIPanel();
		addWidgets2ConstraintPanel();

		resultPanel.add(new JLabel("resultPanel"));
	}

	File selectedfile;
	private void addWidgets2SelectPanel() {

		// library picker
		JButton picker = new JButton("Picker");
		apiListLabel  = new JLabel("API for Library");

		picker.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent ae) {
				JFileChooser chooser = new JFileChooser();
				chooser.setMultiSelectionEnabled(true);
				chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
				chooser.setCurrentDirectory(new File("/Users/burak/Documents/WHarris/safety-itps/SafetyChecker2"));
				int option = chooser.showOpenDialog(new JFrame());
				if (option == JFileChooser.APPROVE_OPTION) {
					File[] sf = chooser.getSelectedFiles();
					String filelist = "nothing";
					if (sf.length > 0)
						filelist = sf[0].getName();
					for (int i = 1; i < sf.length; i++) {

						filelist += ", " + sf[i].getName();
						selectedfile = sf[0];
					}
					LogUtils.warningln("******");
					LogUtils.fatalln(sf.length);
					LogUtils.infoln(sf[0].toString());
					selectedfile = sf[0];
					getAPI();
					//apiListLabel.setText("You chose " + filelist);
				} else {
					apiListLabel.setText("You canceled.");
				}


			}
		});

//		// file selection
//		JFileChooser fileChooser = new JFileChooser();
//		fileChooser.setCurrentDirectory(new File(System.getProperty("user.home")));
//		int result = fileChooser.showOpenDialog(new JFrame());
//		if (result == JFileChooser.APPROVE_OPTION) {
//			File selectedFile = fileChooser.getSelectedFile();
//			System.out.println("Selected file: " + selectedFile.getAbsolutePath());
//		}

		// bound or unbound selection
		String[] parameterTypeArray = { "Bounded Parameters", "UnBounded Parameters" };
		this.parameterTypeBox = new JComboBox(parameterTypeArray);
		this.parameterTypeBox.setSelectedIndex(1);

		selectPanel.setBorder(BorderFactory.createCompoundBorder(
				BorderFactory.createTitledBorder("Select and Configure"), BorderFactory.createEmptyBorder(5, 5, 5, 5)));

		selectPanel.add(picker);
		selectPanel.add(parameterTypeBox);
	}

	JTree tree;
	private void addWidgets2APIPanel() {
		DefaultMutableTreeNode root = new DefaultMutableTreeNode("API");
		createNodes(root);
		tree = new JTree(root);
		tree.addTreeSelectionListener(this);

		JButton button = new JButton("Add to constraint");
		button.addActionListener(new ActionListener() { 
			  public void actionPerformed(ActionEvent e) { 
				      add2ConstraintPressed();
			  } 
		} );

		treeView = new JScrollPane(tree);
		apiPanel.removeAll();
		apiPanel.setLayout(new BoxLayout(apiPanel, BoxLayout.Y_AXIS));
		apiPanel.add(button);
		apiPanel.add(treeView);
		this.frame.pack();

	}

	private void addWidgets2ConstraintPanel() {
		constraintPanel.setBorder(BorderFactory.createCompoundBorder(
				BorderFactory.createTitledBorder("Build Constraint"), BorderFactory.createEmptyBorder(5, 5, 5, 5)));

		constraintPanel.setLayout(new BoxLayout(constraintPanel, BoxLayout.Y_AXIS));
	}
	
	int constraintCount = 0;
	private void addConstraint(LeafNode lf) {
		String constractorCall = lf.getClassInfo() + " obj" + ++constraintCount + " = new " + lf.getClassInfo() + ";";
	       	String assignStmt = lf.getMethod().getReturnType() + " result" + constraintCount + " = obj." + constraintCount + lf.getMethod().getSubSignature().split(" ")[1];
	
//		String invoke = methodSig.get
		JLabel label = new JLabel(constractorCall);
		JLabel label2 = new JLabel(assignStmt);
		constraintPanel.add(label);
		constraintPanel.add(label2);
		SafetyCheckerGUI.frame.pack();
	}

	private HashMap<String, DefaultMutableTreeNode> classNodeMap;

	private void createNodes(DefaultMutableTreeNode root) {
		classNodeMap = new HashMap<String, DefaultMutableTreeNode>();
	
		for(LeafNode lf : api) {
			if(lf.getClassInfo().contains("$"))
				continue;
//			String[] classAndMethod = methodSig.split(":");
//			String classInfo = classAndMethod[0];
//			String methodInfo = classAndMethod[1];

			String classInfo = lf.getClassInfo();

			DefaultMutableTreeNode classNode;
			if(!classNodeMap.containsKey(classInfo)) {
				classNode = new DefaultMutableTreeNode(classInfo);
				classNodeMap.put(classInfo, classNode);
				root.add(classNode);
			} else {
				classNode = classNodeMap.get(classInfo);
			}
			
			DefaultMutableTreeNode methodNode = new DefaultMutableTreeNode(lf);
			classNode.add(methodNode);	
		}
	}

	static JFrame frame = new JFrame("SafetyChecker for Android Apps");
	private static void createAndShowGUI() {

		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setSize(new Dimension(120, 40));

		SafetyCheckerGUI scGUI = new SafetyCheckerGUI(frame.getContentPane());

//		JLabel label = new JLabel("Safety Checker for Android Apps");
		// frame.setContentPane(scGUI.mainPanel);

		JPanel panel = new JPanel(new GridLayout(2, 2));

		frame.pack();
		frame.setVisible(true);
	}

	public static void main(String[] args) {
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				createAndShowGUI();
			}
		});
	}

	LinkedList<LeafNode> api = new LinkedList<LeafNode>();
	private void getAPI() {
		
		api = new LinkedList<LeafNode>();
		Options.v().set_src_prec(Options.src_prec_c);
		Options.v().set_output_format(Options.output_format_shimple);
		Options.v().set_allow_phantom_refs(true);
		String[] sootArgs = new String[] {
				"-process-dir",
				selectedfile.toString(),
				"-output-dir", "src/output/safetyTest" };
		PackManager.v().getPack("stp")
				.add(new Transform("stp.test", new BodyTransformer() {

					@Override
					protected void internalTransform(Body body,
							String phaseName, Map<String, String> options) {
						// hack here
						
						SootMethod method = body.getMethod();
						LeafNode lf = new LeafNode(method);

//						String methodSig = method.getSignature().replaceFirst("<","").replace(">","");
//						System.out.println(methodSig + "---" + body.toString());
						api.add(lf);
					}
				}));
		soot.Main.main(sootArgs);
		LogUtils.fatalln("Size of the api=" + api.size());
		addWidgets2APIPanel();

	}

	public void valueChanged(TreeSelectionEvent e) {
//		DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getLastSelectedPathComponent();
//
//		if(node == null) return;
//
//		Object nodeInfo = node.getUserObject();
//		if(node.isLeaf()) {
//			LogUtils.warningln(node);
//			LogUtils.warningln(nodeInfo);
//			LogUtils.warningln(node.getParent());
//			
//		}
	}

	private void add2ConstraintPressed() {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getLastSelectedPathComponent();

		if(node == null) return;

		Object nodeInfo = node.getUserObject();
		LogUtils.warningln(nodeInfo);
		LogUtils.warningln(nodeInfo.getClass());
		if(node.isLeaf()) {
			String methodSig = nodeInfo.toString();
			String classInfo = node.getParent().toString();
			addConstraint((LeafNode) nodeInfo);
		}

	}

}

class LeafNode {

	SootMethod method;
	String methodSig;
	String classInfo;

	public LeafNode(SootMethod method) {
		this.method = method;	
		String prefix = "";
		if(method.isPublic()) prefix = "public ";
		else if(method.isProtected()) prefix = "protected ";
		else if(method.isPrivate()) prefix = "private ";

		if(method.isStatic()) prefix += "static ";

		String[] methodArray = method.getSignature().replaceFirst("<","").replace(">","").split(":");
		this.classInfo = methodArray[0];
		this.methodSig = prefix +methodArray[1];
	}

	public String getClassInfo() { return this.classInfo; }
	public SootMethod getMethod() { return this.method; }
	public String toString() { return this.methodSig; }


}
