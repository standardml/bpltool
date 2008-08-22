package com.beepell.ui.dom.graph.actions;

import javax.swing.JToolBar;

import com.beepell.ui.dom.graph.DocumentGraph;

/**
 * 
 * @author Tim Hallwyl
 *
 */
public class DocumentGraphlToolBar extends JToolBar  {

    private static final long serialVersionUID = 1L;
    
    /**
     * Create a document graph tool bar.
     * @param panel
     */
    public DocumentGraphlToolBar(DocumentGraph panel) {
        this.setName("Tree Graph Toolbar");
        this.add(new ToggleStateLabelsAction(panel));
        this.add(new ToggleElementNameAction(panel));
        this.add(new ToggleColorsAction(panel));
        this.add(new ToggleIconsAction(panel));
        this.add(new ToggleBordersAction(panel));
        this.add(new ExportImageAction(panel));
    }
  
    
}
