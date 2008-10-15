package com.beepell.tools.application;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.UIManager;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.TreePath;

import com.beepell.Settings;
import com.beepell.execution.ProcessInstance;
import com.beepell.execution.ProcessInstanceListener;
import com.beepell.execution.ProcessInstanceState;
import com.beepell.tools.explorer.XPathExplorer;
import com.beepell.tools.tracker.LogHandler;
import com.beepell.tools.tracker.Tracker;

/**
 * @author Tim Hallwyl
 *
 */
public class Application extends JFrame implements ProcessInstanceListener {

    private final JMenuBar menuBar;

    private final JToolBar toolBar;
    
    private final JSplitPane splitPane;
    
    private final JTree tree;
    
    private final LogHandler logHandler = new LogHandler();
    private final JTable logTable = new JTable(logHandler);
    
    private final Action deployAction = new DeployAction(this); 
    private final Action undeployAction = new UndeployAction(this);
    private final Action invokeAction = new InvokeAction(this);
    
    private final JScrollPane logScrollPane = new JScrollPane(logTable);
       
    private Application() {
        setSize(new Dimension(800, 600));
        setLocationByPlatform(true);
        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("beepell - Deployment Center");
        
        this.menuBar = new JMenuBar();

        this.toolBar = new JToolBar();
        this.toolBar.add(this.deployAction);
        this.toolBar.add(this.undeployAction);
        this.toolBar.add(this.invokeAction);
        //this.invokeAction.setEnabled(false);
        
        this.splitPane = new JSplitPane();
        this.tree = new JTree(new ServerTreeModel());
        this.tree.setCellRenderer(new ServerTreeCellRenderer());

        logTable.getColumnModel().getColumn(0).setMaxWidth(70);
        logTable.getColumnModel().getColumn(0).setMinWidth(20);
        logTable.getColumnModel().getColumn(0).setPreferredWidth(30);

        logTable.getColumnModel().getColumn(1).setMaxWidth(120);
        logTable.getColumnModel().getColumn(1).setMinWidth(50);
        logTable.getColumnModel().getColumn(1).setPreferredWidth(90);
        
        logTable.getColumnModel().getColumn(2).setMaxWidth(100);
        logTable.getColumnModel().getColumn(2).setMinWidth(30);
        logTable.getColumnModel().getColumn(2).setPreferredWidth(70);
        
        logTable.setBackground(Color.WHITE);
        logTable.setShowGrid(false);
        logTable.doLayout();

        logScrollPane.getViewport().setBackground(Color.WHITE);
        logScrollPane.getViewport().setOpaque(true);
        
        Settings.getInstance().getLogger().addHandler(logHandler);
        
        this.splitPane.setLeftComponent(tree);
        this.splitPane.setRightComponent(logScrollPane);
        this.splitPane.setDividerLocation(200);
        
        //this.add(menuBar);
        this.add(toolBar, BorderLayout.PAGE_START);
        this.add(splitPane);
        
        MouseListener ml = new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                int selRow = tree.getRowForLocation(e.getX(), e.getY());
                TreePath selPath = tree.getPathForLocation(e.getX(), e.getY());
                if(selRow != -1) {
                    if(e.getClickCount() == 1) {
                        //mySingleClick(selRow, selPath);
                    }
                    else if(e.getClickCount() == 2) {
                        for (Tracker tracker : Tracker.getTrackers()) {
                            if (tracker.getProcessInstance().equals(selPath.getLastPathComponent()))
                                tracker.setVisible(true);
                        }
                        
                    }
                }
            }
        };

        this.tree.addMouseListener(ml);
        ProcessInstance.addGlobalListener(this);
        
    }
    
    public Object getSelection() {
        return this.tree.getSelectionPath().getLastPathComponent();
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());

            java.awt.EventQueue.invokeLater(new Runnable() {

                public void run() {
                    Settings settings = Settings.getInstance();
                    settings.setSetting("tools.tracker.enabled", "true");
                    settings.setSetting("tools.tracker.mode", "step");
                    new Application().setVisible(true);
                }
            });
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    /* (non-Javadoc)
     * @see com.beepell.execution.ProcessInstanceListener#stateChange(com.beepell.execution.ProcessInstance, com.beepell.execution.ProcessInstanceState, com.beepell.execution.ProcessInstanceState)
     */
    @Override
    public void stateChange(ProcessInstance instance, ProcessInstanceState oldState, ProcessInstanceState newState) {
        ServerTreeModel.update(instance);
        int rows = this.tree.getRowCount();
        for (int i = 0; i < rows; i++)
          this.tree.expandRow(i);
    }

    /* (non-Javadoc)
     * @see com.beepell.execution.ProcessInstanceListener#stepExpected(com.beepell.execution.ProcessInstance)
     */
    @Override
    public void stepExpected(ProcessInstance instance) {
        // do nothing
        
    }


}
