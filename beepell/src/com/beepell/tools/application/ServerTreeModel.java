package com.beepell.tools.application;

import java.util.ArrayList;
import java.util.List;

import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import com.beepell.deployment.DeploymentManager;
import com.beepell.deployment.ProcessContext;

/**
 * @author Tim Hallwyl
 *
 */
public class ServerTreeModel implements TreeModel {

    private static final Object root = "Server (localhost)";
    
    private static final List<TreeModelListener> listeners = new ArrayList<TreeModelListener>();
    
    public static void update(Object source) {
        TreeModelEvent event = new TreeModelEvent(source, new Object[] {root});
        for (TreeModelListener listener : listeners) {
            listener.treeStructureChanged(event);
        }
    }
    
    /* (non-Javadoc)
     * @see javax.swing.tree.TreeModel#addTreeModelListener(javax.swing.event.TreeModelListener)
     */
    @Override
    public void addTreeModelListener(TreeModelListener l) {
        this.listeners.add(l);        
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeModel#getChild(java.lang.Object, int)
     */
    @Override
    public Object getChild(Object parent, int index) {
        if (parent == root)
            return DeploymentManager.getDeployedProcesses().get(index);
        
        if (parent instanceof ProcessContext)
            return ((ProcessContext) parent).getInstances().get(index);
                
        return null;
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeModel#getChildCount(java.lang.Object)
     */
    @Override
    public int getChildCount(Object parent) {
        if (parent == root)
            return DeploymentManager.getDeployedProcesses().size();
        
        if (parent instanceof ProcessContext)
            return ((ProcessContext) parent).getInstances().size();
                
        return 0;
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeModel#getIndexOfChild(java.lang.Object, java.lang.Object)
     */
    @Override
    public int getIndexOfChild(Object parent, Object child) {
        if (parent == root)
            return DeploymentManager.getDeployedProcesses().indexOf(child);
        
        if (parent instanceof ProcessContext)
            return ((ProcessContext) parent).getInstances().indexOf(child);
                
        return 0;
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeModel#getRoot()
     */
    @Override
    public Object getRoot() {
        return root;
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeModel#isLeaf(java.lang.Object)
     */
    @Override
    public boolean isLeaf(Object node) {
        if (node == root)
            return false;
        
        if (node instanceof ProcessContext)
            return false;
                
        return true;
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeModel#removeTreeModelListener(javax.swing.event.TreeModelListener)
     */
    @Override
    public void removeTreeModelListener(TreeModelListener l) {
        this.listeners.remove(l);        
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeModel#valueForPathChanged(javax.swing.tree.TreePath, java.lang.Object)
     */
    @Override
    public void valueForPathChanged(TreePath path, Object newValue) {
        /* ignore: we do not support changed values */
        
    }

    
    
}
