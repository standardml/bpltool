package com.beepell.ui.dom.graph;

import java.util.List;

import org.w3c.dom.Element;

/**
 * Node representing an Element without child Elements.
 * 
 * @author Tim Hallwyl
 * 
 */
public class BasicNode extends TreeNode {

    /**
     * Create a basic activity node.
     * 
     * @param element The element represented by this node.
     */
    BasicNode(Element element) {
        super(element);
    }

    @Override
    public TreeNode getChild(int index) {
        return null;
    }

    @Override
    public int getChildCount() {
        return 0;
    }

    @Override
    public int getIndexOfChild(Object child) {
        return -1;
    }

    @Override
    public boolean isLeaf() {
        return true;
    }

    @Override
    public String toString() {
        return this.element.getLocalName();
    }

    @Override
    public int getWidth() {
        return 70;
    }

    @Override
    public int getHeight() {
        return 70;
    }

    @Override
    public List<TreeNode> getChildren() {
        return null;
    }

    @Override
    public Element getElement() {
        return this.element;
    }
}
