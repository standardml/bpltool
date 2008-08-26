package com.beepell.ui.dom.graph.actions;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;

import com.beepell.ui.dom.graph.DocumentGraph;

/**
 * @author Tim Hallwyl
 * 
 */
public class StateVisibilityMenu extends JMenu implements ActionListener {

    private static final long serialVersionUID = 1L;
    private final DocumentGraph documentGraph;

    /**
     * Create a (sub) menu to toggle state visibility.
     * 
     * @param documentGraph
     */
    public StateVisibilityMenu(DocumentGraph documentGraph) {
        this.documentGraph = documentGraph;
        this.setText("States");

        Map<String, ImageIcon> emblems = this.documentGraph.getConfiguration().getStateEmblems();

        for (String state : emblems.keySet()) {
            JCheckBoxMenuItem menuItem = new JCheckBoxMenuItem();
            if (this.documentGraph.isVisible(state))
                menuItem.setSelected(true);
            else
                menuItem.setSelected(false);
            menuItem.setActionCommand(state);
            menuItem.setText(state);
            menuItem.setIcon(emblems.get(state));
            menuItem.addActionListener(this);
            this.add(menuItem);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        String command = event.getActionCommand();

        if (this.documentGraph.isVisible(command))
            this.documentGraph.hide(command);
        else
            this.documentGraph.show(command);

    }

}
