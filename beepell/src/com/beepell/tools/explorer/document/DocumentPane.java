package com.beepell.tools.explorer.document;

import java.awt.BorderLayout;
import java.awt.Font;
import java.io.File;
import java.io.StringWriter;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreePath;
import javax.xml.namespace.NamespaceContext;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import com.beepell.tools.explorer.XPathExplorer;
import com.beepell.xml.namespace.DocumentNamespaceContext;


/**
 * @author Tim Hallwyl
 *
 */
public class DocumentPane extends JPanel implements TreeSelectionListener {

    private static final long serialVersionUID = 1L;
    
    private final Document document;
    private final DocumentTreeModel documentTreeModel;
    private final DefaultTreeSelectionModel documentSelectionModel;
    
    private final JTextArea xmlTextArea;
    
    private final NamespaceContext namespaceContext;
    
    private File file;
    
    /**
     * 
     * @param document
     * @throws Exception
     */
    public DocumentPane(Document document) throws Exception {
        this.document = document;
        this.documentTreeModel = new DocumentTreeModel(document);
        this.documentSelectionModel = new DefaultTreeSelectionModel();
        this.namespaceContext = new DocumentNamespaceContext(document);
        
        this.setLayout(new BorderLayout());
        final JTree tree = new JTree(documentTreeModel);
        tree.setSelectionModel(documentSelectionModel);
        tree.setCellRenderer(new DocumentTreeCellRenderer());
        tree.addTreeSelectionListener(this);
        tree.setRootVisible(false);

        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitPane.setDividerLocation(200);

        JScrollPane treePane = new JScrollPane(tree);
        splitPane.setLeftComponent(treePane);

        this.xmlTextArea = new JTextArea();
        xmlTextArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
        xmlTextArea.setEditable(false);
        JScrollPane xmlPane = new JScrollPane(xmlTextArea);
        splitPane.setRightComponent(xmlPane);

        splitPane.setDividerLocation(300);
        this.add(BorderLayout.CENTER, splitPane);

    }
    
    /**
     * Return a xml fragment string representing node. 
     * @param node
     * @return xml fragmet string
     */
    public static String nodeToString(Node node) {
        
        if (node.getNodeValue() != null) {
            XPathExplorer.log(node.getLocalName() + " = " + node.getNodeValue());
        }
        
        try {
        StringWriter sw = new StringWriter();
        Transformer serializer;
        serializer = TransformerFactory.newInstance().newTransformer();
        serializer.setOutputProperty(OutputKeys.INDENT, "yes");
        serializer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
        serializer.setOutputProperty(OutputKeys.METHOD, "xml");
        serializer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
        serializer.transform(new DOMSource(node), new StreamResult(sw));
        return sw.toString();
        } catch (Exception exception) {
            return "Failed to parse node to String.";
        }
    }
    
    public void valueChanged(TreeSelectionEvent e) {
        try {
            if (e.isAddedPath()) {

                Node node = (Node) e.getPath().getLastPathComponent();

                if (node.getNodeType() == Node.TEXT_NODE) {
                    if (!((Text) node).isElementContentWhitespace())
                        xmlTextArea.setText(node.getNodeValue());
                } else {
                    xmlTextArea.setText(nodeToString(node));
                }
            }
        } catch (Exception exception) {
            exception.printStackTrace();
        }

    }

    /**
     * Gets the document;
     * @return Document
     */
    public Document getDocument() {        
        return document;
    }

    /**
     * Clears the node selection.
     *
     */
    public void clearSelection() {
        this.documentSelectionModel.clearSelection();
        
    }

    /**
     * Gets the TreePath to a node.
     * @param node
     * @return TreePath to a node
     */
    public TreePath getPath(Node node) {        
        return this.documentTreeModel.getPath(node);
    }

    /**
     * Adds path to selection
     * @param path
     */
    public void addSelectionPath(TreePath path) {
        this.documentSelectionModel.addSelectionPath(path);
    }
    
    /**
     * Get the NamespaceContext
     * @return NamespaceContext
     */
    public NamespaceContext getNamespaceContext() {
        return namespaceContext;
    }

    /**
     * Gets the file info in any.
     * @return File or null
     */
    public File getFile() {
        return file;
    }

    /**
     * Set related file.
     * @param file
     */
    public void setFile(File file) {
        this.file = file;
    }
    
}
