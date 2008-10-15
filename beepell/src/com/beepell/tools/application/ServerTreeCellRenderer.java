package com.beepell.tools.application;

import java.awt.Component;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.beepell.deployment.ProcessContext;
import com.beepell.execution.ProcessInstance;

/**
 * Tree Cell Renderer for DocumentTree.
 * 
 * @author Tim Hallwyl
 * 
 */
public class ServerTreeCellRenderer extends DefaultTreeCellRenderer {

    private static final long serialVersionUID = 1L;

    private static final Icon serverIcon = new ImageIcon(ServerTreeCellRenderer.class.getResource("server.png"), "Server");
    private static final Icon processIcon = new ImageIcon(ServerTreeCellRenderer.class.getResource("process.png"), "Server");
    private static final Icon instanceIcon = new ImageIcon(ServerTreeCellRenderer.class.getResource("instance.png"), "Server");

    /**
     * Create a renderer.
     * 
     */
    public ServerTreeCellRenderer() {
        super();
    }

    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean hasFocus) {

        super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus);

        if (value instanceof String) {
            setIcon(serverIcon);
            setText(value.toString());
        }
        
        if (value instanceof ProcessContext) {
            setIcon(processIcon);
            setText(((ProcessContext) value).getDescription().getName());            
        }
        
        if (value instanceof ProcessInstance) {
            setIcon(instanceIcon);
            setText("Instance (" + ((ProcessInstance) value).getState() + ")");            
        }

        return this;
    }

}
