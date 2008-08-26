package com.beepell.ui;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JPopupMenu;

/**
 * A generic mouse adapter to listen for show-popup-menu mouse event and show
 * the popup menu accordingly.
 * 
 * @author Tim Hallwyl
 * 
 */
public class PopupListener extends MouseAdapter {

    private final JPopupMenu popupMenu;

    /**
     * Create a generic mouse adapter listener to show the popup menu.
     * 
     * @param popupMenu
     */
    public PopupListener(JPopupMenu popupMenu) {
        this.popupMenu = popupMenu;
    }

    @Override
    public void mousePressed(MouseEvent event) {
        maybeShowPopup(event);
    }

    @Override
    public void mouseReleased(MouseEvent event) {
        maybeShowPopup(event);
    }

    private void maybeShowPopup(MouseEvent event) {
        if (event.isPopupTrigger()) {
            this.popupMenu.show(event.getComponent(), event.getX(), event.getY());
        }
    }
}
