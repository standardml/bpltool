package com.beepell.tools.explorer.document;

import java.util.ArrayList;

import javax.swing.SwingUtilities;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * XML DOM Document Tree Model.
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

    public Object getChild(Object parent, int index) {
        Node node = (Node) parent;
        return node.getChildNodes().item(index);
    }

    public int getChildCount(Object parent) {
        Node node = (Node) parent;
        return node.getChildNodes().getLength();
    }

    public int getIndexOfChild(Object parent, Object child) {
        NodeList children = ((Node) parent).getChildNodes();

        for (int i = 0; i < children.getLength(); i++)
            if (children.item(i).equals(child))
                return i;

        return -1;
    }

    public Object getRoot() {
        return document;
    }

    public boolean isLeaf(Object node) {
        if (((Node) node).getChildNodes().getLength() == 0)
            return true;
        else
            return false;
    }

    public void valueForPathChanged(TreePath path, Object newValue) {
        // TODO Auto-generated method stub
        fireStructureChanged(new TreeModelEvent(this, path));

    }

    public void addTreeModelListener(TreeModelListener l) {
        listeners.add(l);
    }

    public void removeTreeModelListener(TreeModelListener l) {
        listeners.remove(l);
    }

    private void fireStructureChanged(final TreeModelEvent event) {

        if (SwingUtilities.isEventDispatchThread()) {
            for (int i = 0; i < listeners.size(); i++)
                listeners.get(i).treeStructureChanged(event);
        } else {
            SwingUtilities.invokeLater(new Runnable() {

                public void run() {
                    for (int i = 0; i < listeners.size(); i++)
                        listeners.get(i).treeStructureChanged(event);
                }
            });
        }
    }

    private Object getParent(Object child) {
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
    public TreePath getPath(Object object) {
        ArrayList<Object> path = new ArrayList<Object>();

        while (object != null) {
            path.add(object);
            object = getParent(object);
        }

        return new TreePath(getReverseArray(path));
    }

    private Object[] getReverseArray(ArrayList path) {

        Object[] array = new Object[path.size()];

        int j = 0;
        for (int i = path.size() - 1; i >= 0; i--) {
            array[j++] = path.get(i);

        }
        return array;

    }
}
