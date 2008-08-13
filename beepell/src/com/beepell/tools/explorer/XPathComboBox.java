package com.beepell.tools.explorer;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.NodeList;

import com.beepell.tools.explorer.document.DocumentPane;

/**
 * 
 * @author Tim Hallwyl
 *
 */
public class XPathComboBox extends JComboBox implements KeyListener, ChangeListener {

    private static final long serialVersionUID = 1L;

    private DocumentPane pane;
    private XPath xpath = XPathFactory.newInstance().newXPath();
    
    /**
     * 
     *
     */
    public XPathComboBox() {
        super();

        setEditable(true);
        ((JTextField) getEditor().getEditorComponent()).addKeyListener(this);
        
    }
    
    
    public void keyPressed(KeyEvent e) {}
    public void keyReleased(KeyEvent e) {}
    public void keyTyped(KeyEvent e) {
        
        if (this.pane == null) {
            XPathExplorer.log("No Document selected.");
            return;
        }
        
        if (e.getKeyChar() == KeyEvent.VK_ENTER) {
            try {
                String path = (String) getSelectedItem();
                
                if (path.endsWith("/")) {
                    path = path.substring(0, path.length() - 1);
                    setSelectedItem(path);
                }
                    
                xpath.setNamespaceContext(pane.getNamespaceContext());
                NodeList result = (NodeList) xpath.evaluate((String) getSelectedItem(), pane.getDocument(), XPathConstants.NODESET);
                XPathExplorer.log("Found " + result.getLength() + " nodes.");
                pane.clearSelection();
                
                for (int i = 0; i < result.getLength(); i++) {
                    pane.addSelectionPath(pane.getPath(result.item(i)));
                    XPathExplorer.log(DocumentPane.nodeToString(result.item(i)));
                }
                
                addItem(getSelectedItem());
                
                
            } catch (Exception exception) {
                String message = exception.getCause().getLocalizedMessage();
                XPathExplorer.log(message);
                JOptionPane.showMessageDialog(null, message, "XPath Error", JOptionPane.WARNING_MESSAGE);
            }
        }                    
    }


    public void stateChanged(ChangeEvent e) {
        if (e.getSource() instanceof JTabbedPane) {
            XPathExplorer.log("Document selected.");
            JTabbedPane tabs = (JTabbedPane) e.getSource();
            this.pane = (DocumentPane) tabs.getSelectedComponent();
            
            
            
        } else {
            XPathExplorer.log(e.getSource().toString());
        }
        
    }

    
    
    
    
}
