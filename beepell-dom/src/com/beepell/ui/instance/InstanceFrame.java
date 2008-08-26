package com.beepell.ui.instance;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.io.File;

import javax.swing.Action;
import javax.swing.DefaultListSelectionModel;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;

import com.beepell.ui.dom.graph.DocumentGraph;
import com.beepell.ui.dom.graph.actions.ExportImageAction;
import com.beepell.ui.dom.graph.actions.StateVisibilityMenu;
import com.beepell.ui.dom.graph.actions.ToggleBordersAction;
import com.beepell.ui.dom.graph.actions.ToggleColorsAction;
import com.beepell.ui.dom.graph.actions.ToggleElementNameAction;
import com.beepell.ui.dom.graph.actions.ToggleIconsAction;
import com.beepell.ui.dom.graph.actions.ToggleStateLabelsAction;
import com.beepell.ui.dom.graph.conf.BPELConfiguration;
import com.beepell.ui.dom.step.StepList;
import com.beepell.ui.dom.xml.DocumentTextArea;
import com.beepell.ui.dom.xml.DocumentTreeCellRenderer;
import com.beepell.ui.dom.xml.DocumentTreeModel;
import com.beepell.ui.dom.xml.actions.SaveAction;
import com.beepell.ui.dom.xpath.XPathPanel;
import com.beepell.ui.icon.IconRepository;

/**
 * Frame (window) to display a process instance.
 * 
 * @author Tim Hallwyl
 * 
 */
public class InstanceFrame extends JFrame {

    private static final long serialVersionUID = 1L;

    private final JTabbedPane tabbedPane;

    private final DocumentGraph documentGraph;

    private final JScrollPane documentGraphPane;

    private final JSplitPane splitPane1;

    private final JTree documentTree;

    private final DocumentTreeModel documentTreeModel;

    private final TreeSelectionModel documentTreeSelectionModel;

    private final JScrollPane documentTreePane;

    private final DocumentTextArea documentTextArea;

    private final JScrollPane documentTextAreaPane;

    private final JPanel xPathAndTextPanel = new JPanel();

    private final JSplitPane splitPane2;

    private final JSplitPane splitPane3;

    private final JToolBar toolBar;

    private final XPathPanel xPathPanel;

    private final ListSelectionModel stepSelectionModel;

    private final StepList stepList1;

    private final StepList stepList2;

    private boolean stepListVisible = true;

    private int split1divider = 200;
    private int split2divider = 200;
    private int split3divider = 400;

    // Actions    
    private final Action stateLabelsAction;
    private final Action elementNameAction;
    private final Action colorsAction;
    private final Action iconsAction;
    private final Action bordersAction;    
    private final Action saveAsXMLAction;
    private final Action stepListAction;
    private final Action exportImageAction;
    
    /**
     * 
     * @param document
     */
    public InstanceFrame(Document document) {

        setSize(new Dimension(930, 700));
        setLocationByPlatform(true);
        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("Process Instance");
        setIconImage(IconRepository.getIcon(IconRepository.APPS, "applications_systemg", IconRepository.TINY).getImage());

        // Setup tabbed pane
        this.tabbedPane = new JTabbedPane(SwingConstants.BOTTOM);

        // Setup step lists
        this.stepSelectionModel = new DefaultListSelectionModel();
        this.stepSelectionModel.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        this.stepList1 = new StepList();
        this.stepList1.setSelectionModel(this.stepSelectionModel);
        this.stepList2 = new StepList();
        this.stepList2.setSelectionModel(this.stepSelectionModel);

        // Setup document graph
        this.documentGraph = new DocumentGraph(document.getDocumentElement(), BPELConfiguration.getInstance());
        this.documentGraphPane = new JScrollPane(this.documentGraph);

        // Setup split pane one
        this.splitPane1 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        this.splitPane1.setDividerLocation(this.split1divider);
        this.splitPane1.setLeftComponent(new JScrollPane(this.stepList1));
        this.splitPane1.setRightComponent(this.documentGraphPane);

        if (this.stepListVisible) {
            this.tabbedPane.add("Graph", this.splitPane1);
        } else {
            this.tabbedPane.add("Graph", this.documentGraphPane);
        }

        // Setup document tree
        this.documentTreeModel = new DocumentTreeModel(document);
        this.documentTree = new JTree(this.documentTreeModel);
        this.documentTreeSelectionModel = new DefaultTreeSelectionModel();
        this.documentTree.setSelectionModel(this.documentTreeSelectionModel);
        this.documentTree.setCellRenderer(new DocumentTreeCellRenderer());
        this.documentTree.setRootVisible(false);
        this.documentTreePane = new JScrollPane(this.documentTree);

        // Setup document text area
        this.documentTextArea = new DocumentTextArea();
        this.documentTextArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
        this.documentTextArea.setEditable(false);
        this.documentTree.addTreeSelectionListener(this.documentTextArea);
        this.documentTextAreaPane = new JScrollPane(this.documentTextArea);

        // Setup split pane two and three
        this.splitPane2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        this.splitPane2.setDividerLocation(this.split2divider);

        this.splitPane3 = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        this.splitPane3.setBorder(null);
        this.splitPane3.setDividerLocation(this.split3divider);
        this.splitPane3.setTopComponent(this.documentTreePane);
        this.splitPane3.setBottomComponent(new JScrollPane(this.stepList2));

        if (this.stepListVisible) {
            this.splitPane2.setLeftComponent(this.splitPane3);
        } else {
            this.splitPane2.setLeftComponent(this.documentTreePane);
        }

        this.xPathPanel = new XPathPanel(this.documentTreeModel, this.documentTreeSelectionModel);
        this.xPathAndTextPanel.setLayout(new BorderLayout());
        this.xPathAndTextPanel.add(this.xPathPanel, BorderLayout.NORTH);
        this.xPathAndTextPanel.add(this.documentTextAreaPane, BorderLayout.CENTER);

        this.splitPane2.setRightComponent(this.xPathAndTextPanel);
        this.tabbedPane.add("XML", this.splitPane2);
       

        // Setup actions
        this.stateLabelsAction = new ToggleStateLabelsAction(this.documentGraph);
        this.elementNameAction = new ToggleElementNameAction(this.documentGraph);
        this.colorsAction = new ToggleColorsAction(this.documentGraph);
        this.iconsAction = new ToggleIconsAction(this.documentGraph);
        this.bordersAction = new ToggleBordersAction(this.documentGraph);    
        this.saveAsXMLAction = new SaveAction(this.documentTextArea);
        this.stepListAction = new ToggleStepListAction(this);
        this.exportImageAction = new ExportImageAction(this.documentGraph);
        this.documentGraph.set(this.getPopupMenu());

        // Setup toolbar
        this.toolBar = new JToolBar();
        this.toolBar.add(this.saveAsXMLAction);

        JToggleButton stepListButton = new JToggleButton(this.stepListAction);
        stepListButton.setHideActionText(true);
        this.toolBar.add(stepListButton);
        
        // Setup frame layout
        this.add(this.tabbedPane);
        this.add(this.toolBar, BorderLayout.PAGE_START);
        TreePath path = this.documentTreeModel.getPath(document.getDocumentElement());
        this.documentTreeSelectionModel.addSelectionPath(path);
        this.documentTree.expandPath(path);

    }

    /**
     * Returns true if the step list is visible, otherwise false.
     * 
     * @return the stepListVisible
     */
    public boolean isStepListVisible() {
        return this.stepListVisible;
    }

    /**
     * @param stepListVisible the stepListVisible to set
     */
    public void setStepListVisible(final boolean stepListVisible) {

        if (stepListVisible != this.stepListVisible) {

            java.awt.EventQueue.invokeLater(new Runnable() {
                @SuppressWarnings("synthetic-access")
                public void run() {

                    InstanceFrame.this.stepListVisible = stepListVisible;

                    int selectedTab = InstanceFrame.this.tabbedPane.getSelectedIndex();

                    if (InstanceFrame.this.stepListVisible) {
                        // turn on step list

                        InstanceFrame.this.split2divider = InstanceFrame.this.splitPane2.getDividerLocation();

                        // GRAPH TAB
                        InstanceFrame.this.splitPane1.setLeftComponent(new JScrollPane(InstanceFrame.this.stepList1));
                        InstanceFrame.this.splitPane1.setRightComponent(InstanceFrame.this.documentGraphPane);
                        InstanceFrame.this.tabbedPane.insertTab("Graph", null, InstanceFrame.this.splitPane1, null, 0);

                        // XML TAB
                        InstanceFrame.this.splitPane3.setBorder(null);
                        InstanceFrame.this.splitPane3.setTopComponent(InstanceFrame.this.documentTreePane);
                        InstanceFrame.this.splitPane3.setBottomComponent(new JScrollPane(InstanceFrame.this.stepList2));
                        InstanceFrame.this.splitPane2.setLeftComponent(InstanceFrame.this.splitPane3);

                        // Reset divider locations
                        InstanceFrame.this.splitPane1.setDividerLocation(InstanceFrame.this.split1divider);
                        InstanceFrame.this.splitPane2.setDividerLocation(InstanceFrame.this.split2divider);
                        InstanceFrame.this.splitPane3.setDividerLocation(InstanceFrame.this.split3divider);

                    } else {
                        // turn off step list
                        InstanceFrame.this.split1divider = InstanceFrame.this.splitPane1.getDividerLocation();
                        InstanceFrame.this.split2divider = InstanceFrame.this.splitPane2.getDividerLocation();
                        if (InstanceFrame.this.splitPane3.getDividerLocation() != 0)
                            InstanceFrame.this.split3divider = InstanceFrame.this.splitPane3.getDividerLocation();

                        InstanceFrame.this.tabbedPane.insertTab("Graph", null, InstanceFrame.this.documentGraphPane, null, 0);
                        InstanceFrame.this.tabbedPane.removeTabAt(1);

                        InstanceFrame.this.splitPane2.setLeftComponent(InstanceFrame.this.documentTreePane);
                        InstanceFrame.this.splitPane2.setDividerLocation(InstanceFrame.this.split2divider);
                        InstanceFrame.this.splitPane2.setDividerLocation(InstanceFrame.this.split2divider);
                    }

                    InstanceFrame.this.tabbedPane.setSelectedIndex(selectedTab);

                }
            });
        }
    }

    /**
     * Shows frame for testing
     * 
     * @param args no arguments needed
     */
    public static void main(String[] args) {

        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception exception) {
            exception.printStackTrace();
            System.exit(ERROR);
        }

        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                try {
                    new InstanceFrame(InstanceFrame.load(new File("samples/advanced.bpel"))).setVisible(true);
                } catch (Exception exception) {
                    exception.printStackTrace();
                    System.exit(ERROR);
                }

            }
        });

    }

    /**
     * Loads an XML file into a DOM Document.
     * 
     * @param file
     * @return a W3C DOM Document
     */
    public static Document load(File file) {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setValidating(false);
            factory.setCoalescing(false);
            factory.setIgnoringComments(true);
            factory.setIgnoringElementContentWhitespace(true);
            factory.setNamespaceAware(true);
            DocumentBuilder builder = factory.newDocumentBuilder();
            final Document document = builder.parse(file);
            document.normalizeDocument();
            return document;
        } catch (Exception exception) {
            exception.printStackTrace();
            return null;
        }
    }
    
    private JPopupMenu getPopupMenu() {
        
        JPopupMenu menu = new JPopupMenu("Graph Panel");
        
        menu.add(new JCheckBoxMenuItem(this.elementNameAction));
        menu.add(new JCheckBoxMenuItem(this.stateLabelsAction));
        menu.add(new JCheckBoxMenuItem(this.colorsAction));
        menu.add(new JCheckBoxMenuItem(this.bordersAction));
        menu.add(new JCheckBoxMenuItem(this.iconsAction));
        menu.addSeparator();
        menu.add(new StateVisibilityMenu(this.documentGraph));        
        menu.addSeparator();
        menu.add(new JCheckBoxMenuItem(this.exportImageAction));
        return menu;
        
    }
    
}
