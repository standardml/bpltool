package com.beepell.tools.tracker.graph;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;

/**
 * 
 * @author Tim Hallwyl
 *
 */
public class TreePanelToolBar extends JToolBar implements ActionListener {

    private static final long serialVersionUID = 1L;
    
    private final InstanceTreePanel panel;
    
    private final JButton eps;
    
    /**
     * 
     * @param panel
     */
    public TreePanelToolBar(InstanceTreePanel panel) {
        this.panel = panel;
        
        eps = new JButton();
        eps.setActionCommand("Save EPS");
        eps.setToolTipText("Save as EPS");
        eps.addActionListener(this);
        eps.setIcon(new ImageIcon(TreePanelToolBar.class.getResource("eps.png"), "Save as EPS"));
        this.add(eps);

        JToggleButton stateLabels = new JToggleButton();
        if (this.panel.drawStateLabels())
            stateLabels.setSelected(true);
        else
            stateLabels.setSelected(false);
        stateLabels.setActionCommand("toggle state labels");
        stateLabels.setToolTipText("State Lables");
        stateLabels.addActionListener(this);
        stateLabels.setIcon(new ImageIcon(TreePanelToolBar.class.getResource("state.png"), "State Labels"));
        this.add(stateLabels);

        JToggleButton useColors = new JToggleButton();
        if (this.panel.useColors())
            useColors.setSelected(true);
        else
            useColors.setSelected(false);
        useColors.setActionCommand("toggle colors");
        useColors.setToolTipText("State Colors");
        useColors.addActionListener(this);
        useColors.setIcon(new ImageIcon(TreePanelToolBar.class.getResource("colors.png"), "State Colors"));
        this.add(useColors);
        
    }

    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if ("Save EPS".equals(command)) {
            this.panel.saveToFile();
            return;
        }
        
        if ("toggle state labels".equals(command)) {
            this.panel.setDrawStateLabels(!this.panel.drawStateLabels());
            return;
        }

        if ("toggle colors".equals(command)) {
            this.panel.setUseColors(!this.panel.useColors());
            return;
        }
        
    }
    
    

}
