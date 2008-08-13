package com.beepell.tools.explorer.document;

import java.awt.Component;

import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Tree Cell Renderer for DocumentTree.
 * @author Tim Hallwyl
 *
 */
public class DocumentTreeCellRenderer extends DefaultTreeCellRenderer {

    private static final long serialVersionUID = 1L;
    
    private static final String[] typeName = {
        "none",
        "Element",
        "Attr",
        "Text",
        "CDATA",
        "EntityRef",
        "Entity",
        "ProcInstr",
        "Comment",
        "Document",
        "DocType",
        "DocFragment",
        "Notation",
    };
    
    /**
     * Create a renderer.
     *
     */    
    public DocumentTreeCellRenderer() {
        super();
    }
    
    public Component getTreeCellRendererComponent(JTree tree, Object value,
            boolean selected, boolean expanded, boolean leaf, int row,
            boolean hasFocus) {
        
        super.getTreeCellRendererComponent(tree, value, selected, expanded,
                leaf, row, hasFocus);
       
        Node node = (Node) value;
        if (node.getNodeType() == Node.ELEMENT_NODE) {
            Element element = (Element) node;
            if (element.hasAttribute("name"))
              setText(node.getNodeName() + " (" + element.getAttribute("name") + ")");
            else
              setText(node.getNodeName());
        } else
          setText(typeName[node.getNodeType()]);
        
        return this;
    }

}
