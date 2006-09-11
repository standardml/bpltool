package dk.itu.bpl.dix.util;

import org.apache.log4j.Logger;
import org.planx.xmlstore.*;

import java.util.*;
import org.planx.xpath.*;
import org.planx.xpath.object.*;



public class NodeUtil {
  private static final Logger logger = Logger.getLogger(NodeUtil.class);
  
  /**
   * Compares the Path returned by <code>DocNode.getPath()</code>
   *  of two nodes and determines whether the presumed parent node
   *  really is a parent to the presumed child node. 
   * @param The presumed parent node
   * @param The presumed child node
   * @return The index of the childs path where the 
   * relative path to the parent begins. If the two nodes are
   * siblings -1 is returned. If the child isn't a child of nor a
   * sibling to the parent -2 is returned.  
   */
  public static int isChildOf(DocNode p, DocNode c){
    int[] pPath = p.getPath();
    int[] cPath = c.getPath();
    int i = 0;
    for(; i < pPath.length; i++) {
      if(i == cPath.length) {
        return -2;
      }
      if(pPath[i] != cPath[i]) {
        return -2;      
      }
    }
    if(i == cPath.length) {
      //the two nodes are siblings
      return -1;
    }
    return i;      
  }
  
  
  /**
   * 
   * @param The node to update
   * @param newn the node to insert
   * @param path an array containing the path to the 
   * Node which should be replaced
   * @param i the index <code>path</code> where the path starts
   * @return The Node with the replaced child.
   */
  public static Node replace(Node root, Node newn, int[] path, int i) {
    if(i == (path.length)) {
      return newn;
    }
    Node child = root.getChildren().get(path[i]);
    return NodeFactory.instance().replaceChild(root, path[i], replace(child, newn, path,i+1));
  }

  public static String nodeInOneLine(Node n) {
    StringBuffer buf = new StringBuffer();
    buf.append("<");
    buf.append(n.getNodeValue());
    List<Attribute> at = n.getAttributes();
    for(Attribute a : at) {
      buf.append(" ");
      buf.append(a.getName());
      buf.append("=");
      buf.append(a.getValue());
    }
    List<? extends Node> childs = n.getChildren();
    if(!childs.isEmpty()) {
      buf.append(" ....");
    }
    buf.append("/>");
    return buf.toString();
  }
  
  public static String printNode(Node n, String tab) {
        StringBuffer buf = new StringBuffer();
        buf.append("\n" + tab + "<" + n.getNodeValue());
        List<Attribute> attributes = n.getAttributes();
        for(Attribute a : attributes) {
            buf.append(" " + a.getName() + "=" + "\"" + a.getValue() + "\"");
        }
        List<? extends Node> childs = n.getChildren();
        if(childs.size() == 0) {
            buf.append("/>");
        } else {
            buf.append(">");
            for(Node no : childs) {
                buf.append(printNode(no, tab + "  "));
            }
            buf.append("\n" + tab + "</" + n.getNodeValue() + ">");
        }
        return buf.toString();
    }
    
    public static XNodeSet getNodes(String path, Node n) {
        try {
            XPath xpath = new XPath(path, new XMLStoreNavigator());
            return (XNodeSet) xpath.evaluate(new DocNode(n));
        } catch (XPathException exp) {
            return null;
        }
    }
    public static Node getNode(String path, Node n) {
        try {
            XPath xpath = new XPath(path, new XMLStoreNavigator());
            XNodeSet set = (XNodeSet) xpath.evaluate(new DocNode(n));
            if(set.size() == 0)
                return null;
            if(set.size() > 1 ) throw new IllegalArgumentException("The result of evaluating the XPath expression: \"" + path +"\" contained more than one Node");
            Node res = (Node) set.get(0);
            return res;
        } catch (XPathException exp) {
            return null;
        }
    }
    
    public static Node getNode(Node root, int[] path){
      Node res = root;
      for(int i = 0; i < path.length; i++) {
        res = getNode(res, path[i]);
      }
      return res;
    }
    
    private static Node getNode(Node root, int childNo) {
      return root.getChildren().get(childNo);
    }
}
