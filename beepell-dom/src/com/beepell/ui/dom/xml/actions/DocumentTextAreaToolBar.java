package com.beepell.ui.dom.xml.actions;

import javax.swing.JToolBar;

import com.beepell.ui.dom.xml.DocumentTextArea;

/**
 * @author Tim Hallwyl
 *
 */
public class DocumentTextAreaToolBar extends JToolBar {

    private static final long serialVersionUID = 1L;
    
    /**
     * Create a new document text area tool bar.
     * @param documentTextArea
     */
    public DocumentTextAreaToolBar(DocumentTextArea documentTextArea) {
        this.setName("XML Toolbar");
        this.add(new SaveAction(documentTextArea));
    }

}
