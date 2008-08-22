package com.beepell.ui.instance;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;

import com.beepell.ui.dom.graph.DocumentGraph;

/**
 * @author Tim Hallwyl
 *
 */
public class StateToolBar extends JToolBar  implements ActionListener {

    private static final long serialVersionUID = 1L;

    private final DocumentGraph documentGraph;
    
    private static final Dimension size = new Dimension(48, 48);
    
    /**
     * Create a toggle state visibility tool bar.
     * @param documentGraph
     */
    public StateToolBar(DocumentGraph documentGraph) {
        this.documentGraph = documentGraph;
        this.setName("Toggle State Visibility");
        
        Map<String, ImageIcon> emblems = documentGraph.getConfiguration().getStateEmblems();
        for (String state : emblems.keySet()) {
            this.add(createButton(state, emblems.get(state)));
        }        
    }
    
    
    private JToggleButton createButton(final String state, final ImageIcon icon) {
        JToggleButton button = new JToggleButton();
        if (this.documentGraph.isVisible(state))
            button.setSelected(true);
        else
            button.setSelected(false);
        button.setActionCommand(state);
        button.setToolTipText("Toggle " + state);
        button.addActionListener(this);
        button.setIcon(icon);
        button.setSize(size);
        button.setPreferredSize(size);
        button.setMinimumSize(size);
        button.setMaximumSize(size);
        return button;
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
