package com.beepell.tools.tracker.graph;

import java.util.ArrayList;
import java.util.List;

import com.beepell.activity.Activity;
import com.beepell.activity.basic.AbstractBasicActivity;
import com.beepell.activity.structured.AbstractStructuredActivity;

/**
 * A structured activity node.
 * @author Tim Hallwyl
 *
 */
public class StructuredNode extends TreeNode {

    private final AbstractStructuredActivity activity;

    private final List<TreeNode> children;

    private int width = 0;
    private int height = 0;

    /**
     * Create a structured activity node.
     * @param activity
     */
    public StructuredNode(AbstractStructuredActivity activity) {
        super(activity);

        this.activity = activity;

        List<Activity> children = activity.getChildren();
        this.children = new ArrayList<TreeNode>(children.size());
        for (Activity child : children) {
            if (child instanceof AbstractBasicActivity)
                this.children.add(new BasicNode((AbstractBasicActivity) child));
            else
                this.children.add(new StructuredNode((AbstractStructuredActivity) child));
        }
    }

    public TreeNode getChild(int index) {
        return this.children.get(index);
    }

    public int getChildCount() {
        return this.children.size();
    }

    public int getIndexOfChild(Object child) {
        return this.children.indexOf(child);
    }

    public boolean isLeaf() {
        return getChildCount() == 0;
    }

    public String toString() {
        return this.activity.getClass().getSimpleName();
    }

    public int getWidth() {
        if (this.width == 0) {
            if (this.children.size() == 0)
                return 70;
            
            for (TreeNode child : this.children) {
                width += child.getWidth();
            }
            
            width += (this.children.size() -1) * 30;
            
        } 
        return width;
    }
    
    public int getHeight() {
        if (this.height == 0) {
            if (this.children.size() == 0)
                return 70;
            
            for (TreeNode child : this.children) {
                height = Math.max(height, child.getHeight());
            }
            
            height += 100;
            
        } 
        return height; 
    }
    
    public List<TreeNode> getChildren() {
        return children;
    }
    
    public Activity getActivity() {
        return this.activity;
    }
}