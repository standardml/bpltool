package com.beepell.tools.tracker;

import java.awt.Component;

import javax.swing.JToolBar;

/**
 * 
 * @author Tim Hallwyl
 *
 */
public class CombinedToolBar extends JToolBar {

    private static final long serialVersionUID = 1L;
    private boolean isEmpty = true; 
    
    /**
     * Add a tool bar to the combined tool bar.
     * @param toolBar
     */
    public void add(JToolBar toolBar) {
        
        if (isEmpty)
            isEmpty = false;
        else
            addSeparator();
        
        Component[] components = toolBar.getComponents();
        for (int index = 0; index < components.length; index++)
            add(components[index]);
        
    }
    
}
