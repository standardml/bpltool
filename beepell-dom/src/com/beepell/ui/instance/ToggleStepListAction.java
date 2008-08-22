package com.beepell.ui.instance;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;

import com.beepell.ui.icon.IconRepository;

/**
 * @author Tim Hallwyl
 * 
 */
public class ToggleStepListAction extends AbstractAction {

    private static final long serialVersionUID = 1L;

    private final InstanceFrame frame;

    /**
     * Create a "toggle show/hide step list" action.
     * 
     * @param frame
     */
    public ToggleStepListAction(InstanceFrame frame) {
        this.frame = frame;
        ImageIcon large = IconRepository.getIcon(IconRepository.ACTIONS, "footprint", IconRepository.MEDIUM);
        ImageIcon small = IconRepository.getIcon(IconRepository.ACTIONS, "footprint", IconRepository.TINY);

        this.putValue(LARGE_ICON_KEY, large);
        this.putValue(SMALL_ICON, small);
        this.putValue(NAME, "Toggle borders");
        this.putValue(SHORT_DESCRIPTION, "Toggle step list");
        this.putValue(LONG_DESCRIPTION, "Toggle step list");

        this.setEnabled(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent arg0) {
        this.frame.setStepListVisible(!this.frame.isStepListVisible());
    }

}
