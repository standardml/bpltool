package com.beepell.ui.dom.xpath;

import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.tree.TreeSelectionModel;

import com.beepell.ui.dom.xml.DocumentTreeModel;

/**
 * A panel with a clear button and a location combo-box.
 * 
 * @author Tim Hallwyl
 * 
 */
public class XPathPanel extends JPanel {

    private static final long serialVersionUID = 1L;

    private final XPathComboBox comboBox;

    /**
     * Create XPath panel.
     * 
     * @param treeModel
     * @param selectionModel
     */
    public XPathPanel(DocumentTreeModel treeModel, TreeSelectionModel selectionModel) {
        this.setName("XPath");
        this.setLayout(new BorderLayout());

        this.comboBox = new XPathComboBox(treeModel, selectionModel);
        Action clearAction = new ClearLocationBarAction(this.comboBox);
        JButton clear = new JButton(clearAction);
        clear.setHideActionText(true);
        clear.setPreferredSize(new Dimension(22, 22));
        clear.setIcon((Icon) clearAction.getValue(Action.SMALL_ICON));
        this.add(clear, BorderLayout.WEST);
        this.add(this.comboBox, BorderLayout.CENTER);
    }

}
