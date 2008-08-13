package com.beepell.tools.tracker.graph;

import java.util.List;

import com.beepell.activity.Activity;
import com.beepell.activity.basic.AbstractBasicActivity;

/**
 * 
 * @author Tim Hallwyl
 *
 */
public class BasicNode extends TreeNode {
    
    private final AbstractBasicActivity activity;

    /**
     * Create a basic activity node.
     * @param activity
     */
    public BasicNode(AbstractBasicActivity activity) {
        super(activity);
        this.activity = activity;
    }
    
    public TreeNode getChild(int index) {        
        return null;
    }

    public int getChildCount() {
        return 0;
    }

    public int getIndexOfChild(Object child) {
        return -1;
    }

    public boolean isLeaf() {
        return true;
    }
    
    public String toString() {
        return this.activity.getClass().getSimpleName();            
    }
    
    public int getWidth() {
        return 70;
    }
    
    public int getHeight() {
        return 70;
    }
    public List<TreeNode> getChildren() {
        return null;
    }
    
    public Activity getActivity() {
        return this.activity;
    }
}
