package com.beepell.ui.dom.graph;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Node representing an Element with child Elements.
 * 
 * @author Tim Hallwyl
 * 
 */
public class StructuredNode extends TreeNode {

    private final List<TreeNode> children;

    private int width = 0;
    private int height = 0;

    /**
     * Create a structured activity node.
     * 
     * @param element the Element represented by this node. 
     * @param visibleElements the list of visible Element QNames
     * 
     */
    StructuredNode(Element element, final List<QName> visibleElements) {
        super(element);

        NodeList nodes = element.getChildNodes();
        List<Element> elements = new ArrayList<Element>();

        for (int i = 0; i < nodes.getLength(); i++) {
            Node node = nodes.item(i);

            if (node instanceof Element) {
                if (visibleElements == null || visibleElements.contains(new QName(node.getNamespaceURI(), node.getLocalName())))
                    elements.add((Element) nodes.item(i));
            }
        }

        this.children = new ArrayList<TreeNode>(elements.size());
        for (Element child : elements) {
            if (hasChildElementNodes(child))
                this.children.add(new StructuredNode(child, visibleElements));
            else
                this.children.add(new BasicNode(child));

        }
    }

    /**
     * Returns whether parent node has any Element children.
     * 
     * @param parent
     * @return true if the parent has at least one Element child node.
     */
    private boolean hasChildElementNodes(Element parent) {
        NodeList nodes = parent.getChildNodes();

        for (int i = 0; i < nodes.getLength(); i++) {
            if (nodes.item(i) instanceof Element)
                return true;
        }

        return false;
    }

    @Override
    public TreeNode getChild(int index) {
        return this.children.get(index);
    }

    @Override
    public int getChildCount() {
        return this.children.size();
    }

    @Override
    public int getIndexOfChild(Object child) {
        return this.children.indexOf(child);
    }

    @Override
    public boolean isLeaf() {
        return getChildCount() == 0;
    }

    @Override
    public String toString() {
        return this.element.getLocalName();
    }

    @Override
    public int getWidth() {
        if (this.width == 0) {
            if (this.children.size() == 0)
                return 70;

            for (TreeNode child : this.children) {
                this.width += child.getWidth();
            }

            this.width += (this.children.size() - 1) * 30;

        }
        return this.width;
    }

    @Override
    public int getHeight() {
        if (this.height == 0) {
            if (this.children.size() == 0)
                return 70;

            for (TreeNode child : this.children) {
                this.height = Math.max(this.height, child.getHeight());
            }

            this.height += 100;

        }
        return this.height;
    }

    @Override
    public List<TreeNode> getChildren() {
        return this.children;
    }

    @Override
    public Element getElement() {
        return this.element;
    }
}