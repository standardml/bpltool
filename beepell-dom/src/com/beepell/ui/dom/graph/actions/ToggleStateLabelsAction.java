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
public class ToggleStateLabelsAction extends AbstractAction {

    /**
     * 
     */

    private static final long serialVersionUID = 1L;

    private final DocumentGraph panel;

    /**
     * Create a "toggle show state text labels" action.
     * 
     * @param panel the DocumentGraph to control.
     */
    public ToggleStateLabelsAction(DocumentGraph panel) {
        this.panel = panel;
        ImageIcon large = IconRepository.getIcon(IconRepository.ACTIONS, "rss_tag", IconRepository.MEDIUM);
        ImageIcon small = IconRepository.getIcon(IconRepository.ACTIONS, "rss_tag", IconRepository.TINY);

        this.putValue(LARGE_ICON_KEY, large);
        this.putValue(SMALL_ICON, small);
        this.putValue(NAME, "Toggle states lables");
        this.putValue(SHORT_DESCRIPTION, "Toggle state labels");
        this.putValue(LONG_DESCRIPTION, "Toggle state lables");
        this.putValue(SELECTED_KEY, Boolean.valueOf(panel.isStateLabelsVisible()));

        this.setEnabled(true);
        
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent e) {

        this.panel.setStateLabelsVisible(!this.panel.isStateLabelsVisible());

    }

}