package com.beepell.ui.dom.graph.actions;

import javax.swing.JToggleButton;
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
        
        ToggleStateLabelsAction stateLabelsAction = new ToggleStateLabelsAction(panel);
        JToggleButton stateLabelsButton = new JToggleButton(stateLabelsAction);
        stateLabelsButton.setHideActionText(true);
        this.add(stateLabelsButton);

        ToggleElementNameAction elementNameAction = new ToggleElementNameAction(panel);
        JToggleButton elementNameButton = new JToggleButton(elementNameAction);
        elementNameButton.setHideActionText(true);
        this.add(elementNameButton);

        ToggleColorsAction colorsAction = new ToggleColorsAction(panel);
        JToggleButton colorsButton = new JToggleButton(colorsAction);
        colorsButton.setHideActionText(true);
        this.add(colorsButton);

        ToggleIconsAction iconsAction = new ToggleIconsAction(panel);
        JToggleButton iconsButton = new JToggleButton(iconsAction);
        iconsButton.setHideActionText(true);
        this.add(iconsButton);
        
        ToggleBordersAction bordersAction = new ToggleBordersAction(panel);
        JToggleButton bordersButton = new JToggleButton(bordersAction);
        bordersButton.setHideActionText(true);
        this.add(bordersButton);

        this.add(new ExportImageAction(panel));
    }
  
    
}
