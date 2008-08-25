package com.beepell.ui.dom.xml;

import java.util.ArrayList;
import java.util.List;

import javax.swing.SwingUtilities;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Document Tree Model to model DOM Element nodes as a JTree. Note: While this
 * implementation allows changing the Document, it has no mechanism to detect or
 * get notified about smaller changes.
 * 
 * @author Tim Hallwyl
 * 
 */
public class DocumentTreeModel implements TreeModel {

    /**
     * The DOM document.
     */
    private Document document;

    /**
     * List of TreeModelListeners.
     */
    private ArrayList<TreeModelListener> listeners = new ArrayList<TreeModelListener>();

    /**
     * Create a model based on a DOM document.
     * 
     * @param document
     */
    public DocumentTreeModel(Document document) {
        this.document = document;
    }

    /**
     * Change the Document of the model.
     * 
     * @param document new Document
     */
    public void setDocument(Document document) {
        this.document = document;
        fireStructureChanged(new TreeModelEvent(this, new Object[] {}));
    }

    private List<Element> getChildren(Element parent) {
        List<Element> children = new ArrayList<Element>();
        NodeList nodes = parent.getChildNodes();
        for (int i = 0; i < nodes.getLength(); i++) {
            if (nodes.item(i) instanceof Element)
                children.add((Element) nodes.item(i));
        }
        return children;
    }

    public Object getChild(Object parent, int index) {
        if (parent instanceof Document)
            return this.document.getDocumentElement();

        return this.getChildren((Element) parent).get(index);
    }

    public int getChildCount(Object parent) {
        int count = 0;
        NodeList nodes = ((Node) parent).getChildNodes();
        for (int i = 0; i < nodes.getLength(); i++) {
            if (nodes.item(i) instanceof Element)
                count++;
        }
        return count;
    }

    public int getIndexOfChild(Object parent, Object child) {
        return getChildren((Element) parent).indexOf(child);
    }

    public Document getRoot() {
        return this.document;
    }

    public boolean isLeaf(Object node) {
        return (getChildCount(node) == 0);
    }

    public void valueForPathChanged(TreePath path, Object newValue) {
        fireStructureChanged(new TreeModelEvent(this, path));
    }

    public void addTreeModelListener(TreeModelListener l) {
        this.listeners.add(l);
    }

    public void removeTreeModelListener(TreeModelListener l) {
        this.listeners.remove(l);
    }

    private void fireStructureChanged(final TreeModelEvent event) {

        if (SwingUtilities.isEventDispatchThread()) {
            for (int i = 0; i < this.listeners.size(); i++)
                this.listeners.get(i).treeStructureChanged(event);
        } else {
            SwingUtilities.invokeLater(new Runnable() {

                @SuppressWarnings("synthetic-access")
                public void run() {
                    List<TreeModelListener> listeners = DocumentTreeModel.this.listeners;
                    for (int i = 0; i < listeners.size(); i++)
                        listeners.get(i).treeStructureChanged(event);
                }
            });
        }
    }

    private Object getParent(final Object child) {
        if (child instanceof Document)
            return null;

        if (child instanceof Node)
            return ((Node) child).getParentNode();

        return null;
    }

    /**
     * Get the TreePath to the object.
     * 
     * @see TreePath
     * @param object
     * @return TreePath to the object.
     */
    public TreePath getPath(final Object object) {
        ArrayList<Object> path = new ArrayList<Object>();

        Object next = object;
        while (next != null) {
            path.add(next);
            next = getParent(next);
        }

        return new TreePath(getReverseArray(path));
    }

    private Object[] getReverseArray(final ArrayList<Object> path) {

        Object[] array = new Object[path.size()];

        int j = 0;
        for (int i = path.size() - 1; i >= 0; i--) {
            array[j++] = path.get(i);
        }

        return array;

    }
}
