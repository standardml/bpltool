package com.beepell.tools.tracker.graph;

import java.util.List;

import com.beepell.activity.Activity;

/**
 * Abstrac super class for basic and structured nodes.
 * @author Tim Hallwyl
 *
 */
public abstract class TreeNode {
    
    //private static final Hashtable<Activity, TreeNode> nodes = new Hashtable<Activity, TreeNode>();
    
    protected TreeNode(Activity activity) {
        //nodes.put(activity, this);
    }
    
    /*
     * Lookup the TreeNode of the Activity
     * @param activity
     * @return the TreeNode of the Activity
    public static  TreeNode getNode(Activity activity) {
        return nodes.get(activity);
    }
     */

    /**
     * Get the list of children.
     * @return the list of children.
     */
    public abstract List<TreeNode> getChildren();
    
    /**
     * Get the child activity at the index.
     * @param index
     * @return TreeNode representing the child activity at the index.
     */
    public abstract TreeNode getChild(int index);

    /**
     * Number of child activities.
     * @return number of child activities.
     */
    public abstract int getChildCount();

    /**
     * Index of the child.
     * @param child
     * @return index of child.
     */
    public abstract int getIndexOfChild(Object child);
    
    /**
     * True if the node has child nodes, otherwise false.
     * @return True if the node has child nodes
     */
    public abstract boolean isLeaf();
    
    /**
     * the pixel width needed to paint this node and all its children.
     * @return the pixel width
     */
    public abstract int getWidth();
    
    /**
     * x position
     */
    public int x;
    
    /**
     * y posistion
     */
    public int y;
    
    /**
     * Get access to the activity represented by this node
     * @return the activity 
     */
    public abstract Activity getActivity();

    /**
     * Get the height in pixel required by this node.
     * @return the height in pixel
     */
    public abstract int getHeight();
    
}
