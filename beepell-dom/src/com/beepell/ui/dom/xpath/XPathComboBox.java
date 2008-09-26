package com.beepell.ui.dom.xpath;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.tree.TreeSelectionModel;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.beepell.ui.dom.xml.DocumentTreeModel;
import com.beepell.ui.icon.IconRepository;
import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * 
 * @author Tim Hallwyl
 * 
 */
public class XPathComboBox extends JComboBox implements KeyListener {

    private static final long serialVersionUID = 1L;

    private XPath xpath = XPathFactory.newInstance().newXPath();

    private DocumentTreeModel treeModel;
    private final TreeSelectionModel selectionModel;

    /**
     * Create a XPath ComboBox component.
     * 
     * @param treeModel the document tree model
     * @param selectionModel the tree selection model
     * 
     */
    public XPathComboBox(DocumentTreeModel treeModel, TreeSelectionModel selectionModel) {
        super();

        setEditable(true);
        ((JTextField) getEditor().getEditorComponent()).addKeyListener(this);

        this.treeModel = treeModel;
        this.selectionModel = selectionModel;
    }

    public void keyPressed(KeyEvent e) { /* do nothing */
    }

    public void keyReleased(KeyEvent e) { /* do nothing */
    }

    public void keyTyped(KeyEvent e) {

        if (e.getKeyChar() == KeyEvent.VK_ENTER) {
            Document document = this.treeModel.getRoot();
            if (document == null)
                return;

            try {
                String path = (String) getSelectedItem();

                if (path.endsWith("/")) {
                    path = path.substring(0, path.length() - 1);
                    setSelectedItem(path);
                }

                NodeList result = null;
                try {
                    this.xpath.setNamespaceContext(new DocumentNamespaceContext(document));
                    result = (NodeList) this.xpath.evaluate((String) getSelectedItem(), document, XPathConstants.NODESET);
                    
                } catch (XPathExpressionException exception) {
                    // Attempt to validate into a String
                    this.xpath.setNamespaceContext(new DocumentNamespaceContext(document));
                    String string = this.xpath.evaluate((String) getSelectedItem(), document);
                    JOptionPane.showMessageDialog(this, string, "String Result", JOptionPane.INFORMATION_MESSAGE, IconRepository.getInfoIcon());
                    return;
                }

                if (result.getLength() == 0) {
                    JOptionPane.showMessageDialog(this, "The expression did not select any nodes.", "XPath", JOptionPane.WARNING_MESSAGE, IconRepository
                            .getInfoIcon());
                    return;
                }

                this.selectionModel.clearSelection();

                for (int i = 0; i < result.getLength(); i++) {
                    if (!(result.item(i) instanceof Element)) {
                        String title = "Expression Result " + (i + 1) + " of " + result.getLength();
                        JOptionPane.showMessageDialog(this, result.item(i), title, JOptionPane.INFORMATION_MESSAGE, IconRepository.getInfoIcon());
                    } else {
                        this.selectionModel.addSelectionPath(this.treeModel.getPath(result.item(i)));
                    }
                }

                addItem(getSelectedItem());

            } catch (Exception exception) {
                String message = exception.getCause().getLocalizedMessage();
                JOptionPane.showMessageDialog(this, message, "XPath Error", JOptionPane.WARNING_MESSAGE, IconRepository.getWarningIcon());
            }
        }
    }

    /**
     * 
     */
    public void clear() {
        this.selectionModel.clearSelection();
        this.setSelectedItem("");
    }

}
