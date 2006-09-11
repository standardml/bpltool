package dk.itu.bpl.dix.frontend;

import javax.swing.*;
import javax.swing.tree.*;

import org.planx.xmlstore.Attribute;
import org.planx.xmlstore.Node;

import java.util.*;

public class ProcessTree extends JScrollPane {
  JTree tree = null;
  
  
  public ProcessTree(){
    DefaultMutableTreeNode root = new DefaultMutableTreeNode("No process tree loaded");
    tree = new JTree(root);    
  }
  
  public ProcessTree(Node rootNode) {
    DefaultMutableTreeNode root = new DefaultMutableTreeNode("wide");
    List<? extends Node> childs = rootNode.getChildren();
    for(Node c : childs) {
      root.add(getNode(c));
    }
    tree = new JTree(root);
    setViewportView(tree);    
  }
  
  public void updateTree(Node rootNode) {
    DefaultMutableTreeNode root = new DefaultMutableTreeNode("wide");
    List<? extends Node> childs = rootNode.getChildren();
    for(Node c : childs) {
      root.add(getNode(c));
    }
    tree = new JTree(root);
    setViewportView(tree);        
  }
  
  private DefaultMutableTreeNode getNode(Node root) {
    DefaultMutableTreeNode dmt = new DefaultMutableTreeNode(nodeToString(root));
    List<? extends Node> childs = root.getChildren();
    for(Node c : childs) {
      dmt.add(getNode(c));
    }
    return dmt;
  }
  
  private String nodeToString(Node n) {
    StringBuffer buf = new StringBuffer();
    List<? extends Node> childs = n.getChildren();
    buf.append(n.getNodeValue());
    List<Attribute> attributes = n.getAttributes();
    for(int i = 0; i < attributes.size();i++) {
      buf.append(" " + attributes.get(i).getName() + "=" + "\"" + attributes.get(i).getValue() + "\"");
    }
    return buf.toString();

  }

}
