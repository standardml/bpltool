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
public class ToggleIconsAction extends AbstractAction {

    private static final long serialVersionUID = 1L;
    
    private final DocumentGraph panel;
    
    /**
     * Create a "toggle show icons" action.
     * 
     * @param panel the DocumentGraph to control.
     */
    public ToggleIconsAction(DocumentGraph panel) {
        this.panel = panel;
        ImageIcon large = IconRepository.getIcon(IconRepository.ACTIONS, "bookmark_add", IconRepository.MEDIUM);
        ImageIcon small = IconRepository.getIcon(IconRepository.ACTIONS, "bookmark_add", IconRepository.TINY);

        this.putValue(LARGE_ICON_KEY, large);
        this.putValue(SMALL_ICON, small);
        this.putValue(NAME, "Toggle icons");
        this.putValue(SHORT_DESCRIPTION, "Toggle icons");
        this.putValue(LONG_DESCRIPTION, "Toggle icons");

        this.setEnabled(true);
    }

    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent e) {

        this.panel.setDrawIcons(!this.panel.isDrawIcons());
        this.panel.setDrawEmblems(!this.panel.isDrawEmblems());

    }

}
