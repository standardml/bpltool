package com.beepell.ui.dom.graph.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;

import com.beepell.ui.dom.graph.DocumentGraph;
import com.beepell.ui.icon.IconRepository;

/**
 * @author Tim Hallwyl
 *
 */
public class ToggleColorsAction extends AbstractAction {

    private static final long serialVersionUID = 1L;

    private final DocumentGraph panel;
    
    /**
     * Create a "toggle state specific coloring" action.
     * 
     * @param panel the DocumentGraph to control.
     */
    public ToggleColorsAction(DocumentGraph panel) {
        this.panel = panel;
        
        ImageIcon large = IconRepository.getIcon(IconRepository.ACTIONS, "color_line", IconRepository.MEDIUM);
        ImageIcon small = IconRepository.getIcon(IconRepository.ACTIONS, "color_line", IconRepository.TINY);

        this.putValue(LARGE_ICON_KEY, large);
        this.putValue(SMALL_ICON, small);
        this.putValue(NAME, "Toggle colors");
        this.putValue(SHORT_DESCRIPTION, "Toggle state colors");
        this.putValue(LONG_DESCRIPTION, "Toggle state dependent coloring of nodes");
        this.putValue(SELECTED_KEY, Boolean.valueOf(panel.isDrawColors()));
        
        this.setEnabled(true);
    }
    
    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        
        this.panel.setDrawColors(!this.panel.isDrawColors());

    }

}
