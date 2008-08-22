package com.beepell.ui.dom.xml;

import java.awt.Component;

import javax.swing.Icon;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.beepell.ui.icon.IconRepository;

/**
 * Tree Cell Renderer for DocumentTree.
 * 
 * @author Tim Hallwyl
 * 
 */
public class DocumentTreeCellRenderer extends DefaultTreeCellRenderer {

    private static final long serialVersionUID = 1L;

    private static final String[] typeName = { "none", "Element", "Attr", "Text", "CDATA", "EntityRef", "Entity", "ProcInstr", "Comment", "Document",
            "DocType", "DocFragment", "Notation", };

    private static final Icon expandedIcon = IconRepository.getIcon(IconRepository.ACTIONS, "2downarrow", IconRepository.TINY);
    private static final Icon foldedIcon = IconRepository.getIcon(IconRepository.ACTIONS, "2rightarrow", IconRepository.TINY);
    private static final Icon leafIcon = IconRepository.getIcon(IconRepository.ACTIONS, "1rightarrow", IconRepository.TINY);

    /**
     * Create a renderer.
     * 
     */
    public DocumentTreeCellRenderer() {
        super();
    }

    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean hasFocus) {

        super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus);

        if (leaf)
            setIcon(leafIcon);
        else {
            if (expanded)
                setIcon(expandedIcon);
            else
                setIcon(foldedIcon);
        }

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
