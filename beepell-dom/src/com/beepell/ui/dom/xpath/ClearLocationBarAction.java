package com.beepell.ui.dom.xpath;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;

import com.beepell.ui.icon.IconRepository;

/**
 * A "clear XPath location bar" action class.
 * 
 * @author Tim Hallwyl
 * 
 */
public class ClearLocationBarAction extends AbstractAction {

    private static final long serialVersionUID = 1L;

    private final XPathComboBox comboBox;

    /**
     * Create a "clear XPath location bar" action.
     * 
     * @param comboBox
     */
    public ClearLocationBarAction(XPathComboBox comboBox) {
        this.comboBox = comboBox;
        ImageIcon large = IconRepository.getIcon(IconRepository.ACTIONS, "edit_clear_locationbar", IconRepository.MEDIUM);
        ImageIcon small = IconRepository.getIcon(IconRepository.ACTIONS, "edit_clear_locationbar", IconRepository.TINY);

        this.putValue(LARGE_ICON_KEY, large);
        this.putValue(SMALL_ICON, small);
        this.putValue(NAME, "Clear XPath");
        this.putValue(SHORT_DESCRIPTION, "Clear XPath");
        this.putValue(LONG_DESCRIPTION, "Clear the XPath location");

        this.setEnabled(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent arg0) {
        this.comboBox.clear();
    }

}
