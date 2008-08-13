package com.beepell.tools.tracker;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import com.beepell.Settings;
import com.beepell.execution.ProcessInstance;
import com.beepell.tools.tracker.graph.InstanceTreePanel;
import com.beepell.tools.tracker.graph.TreePanelToolBar;

/**
 * @author Tim Hallwyl
 */
public class Tracker extends JFrame implements TableModelListener {

    private static final long serialVersionUID = 1L;
    private final JTable logTable;
    private final JScrollPane logScrollPane;
    /**
     * Create a Process Instance Tracker Frame (window)
     * 
     * @param instance
     */
    public Tracker(ProcessInstance instance) { 
        Settings settings = Settings.getInstance();
        if (settings.getSetting("tools.tracker.maximized", "false").equals("true"))
            setSize(java.awt.Toolkit.getDefaultToolkit().getScreenSize());
        else
            setSize(new Dimension(800, 600));

        
        setLocationByPlatform(true);
        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("Process Instance Tracker");
        
        if (instance.getProcessScope().getName() != null)
            setTitle("Beepell - " + instance.getProcessScope().getName());
        else
            setTitle("Beepell - unnamed"); 
        

        InstanceTreePanel panel = new InstanceTreePanel(instance);
        JScrollPane graphScrollPane = new JScrollPane(panel);
        
        LogHandler logHandler = new LogHandler();
        instance.addLogHandler(logHandler);
        logTable = new JTable(logHandler);

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
        logHandler.addTableModelListener(this);
        logScrollPane = new JScrollPane(logTable);
        
        
        
        CombinedToolBar toolBar = new CombinedToolBar();
        JToolBar instanceBar = new InstanceControlToolBar(instance);
        toolBar.add(instanceBar);        
        JToolBar panelBar = new TreePanelToolBar(panel);
        toolBar.add(panelBar);        
        add(toolBar, BorderLayout.PAGE_START);
        
        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true);
        splitPane.setTopComponent(graphScrollPane);
        splitPane.setBottomComponent(logScrollPane);
        splitPane.setDividerLocation(400);
        splitPane.setDoubleBuffered(true);
        splitPane.setResizeWeight(1D);
        add(splitPane, BorderLayout.CENTER);
        
        

    }
    
    

    /**
     * Create a Process Instance Tracker Frame (window) and sets it visisble.
     * 
     * @param instance
     */
    public static void createInstance(final ProcessInstance instance) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());

            java.awt.EventQueue.invokeLater(new Runnable() {

                public void run() {
                    new Tracker(instance).setVisible(true);
                }
            });
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }



    public void tableChanged(TableModelEvent e) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                logTable.scrollRectToVisible(logTable.getCellRect(logTable.getRowCount()-1,0,true));
            }
        });
    }

}
