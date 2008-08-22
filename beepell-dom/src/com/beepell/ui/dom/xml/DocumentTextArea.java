package com.beepell.ui.dom.xml;

import java.awt.Rectangle;
import java.io.StringWriter;

import javax.swing.JOptionPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Node;
import org.w3c.dom.Text;

import com.beepell.ui.icon.IconRepository;

/**
 * @author Tim Hallwyl
 * 
 */
public class DocumentTextArea extends JTextArea implements TreeSelectionListener {

    private static final long serialVersionUID = 1L;
    
    private Node node;

    /**
     * Get the currently displayed node.
     * 
     * @return the displayed node
     */
    public Node getNode() {
        return this.node;
    }
    
    /**
     * Sets the DOM node to be displayed as XML text.
     * 
     * @param node node to be displayed
     */
    public void setNode(Node node) {
        this.node = node;
        
        if (node == null)
            setText("");
        
        if (this.node.getNodeType() == Node.TEXT_NODE) {
            if (!((Text) this.node).isElementContentWhitespace())
                setText(this.node.getNodeValue());
        } else {
            setText(nodeToString(this.node));
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    scrollRectToVisible(new Rectangle(0, 0, 100, 100));
                }
            });
        }
    }

    /**
     * Implementation of the TreeSelectionListener interface to listen for
     * changed selections in the tree.
     * 
     * @see javax.swing.event.TreeSelectionListener#valueChanged(TreeSelectionEvent)
     */
    public void valueChanged(TreeSelectionEvent event) {
        try {
            if (event.isAddedPath()) {

                Node node = (Node) event.getPath().getLastPathComponent();
                setNode(node);

            }
        } catch (Exception exception) {
            exception.printStackTrace();
        }

    }

    /**
     * Return a xml fragment string representing node.
     * 
     * @param node the node to be converted into a string
     * @return xml fragmet string
     */
    private String nodeToString(Node node) {

        try {
            StringWriter sw = new StringWriter();
            Transformer serializer;
            serializer = TransformerFactory.newInstance().newTransformer();
            serializer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            serializer.setOutputProperty(OutputKeys.INDENT, "yes");
            serializer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
            serializer.setOutputProperty("{http://xml.apache.org/xalan}indent-amount", "4");
            serializer.setOutputProperty(OutputKeys.METHOD, "xml");
            serializer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            serializer.transform(new DOMSource(node), new StreamResult(sw));
            return sw.toString();
        } catch (Exception exception) {
            JOptionPane.showMessageDialog(this, exception.getLocalizedMessage(), "Cannot show XML", ERROR, IconRepository.getCriticalIcon());
            return "";
        }
    }

}
