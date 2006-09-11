package dk.itu.bpl.dix.reactivexml;

import java.util.*;

import org.planx.xmlstore.Node;
import org.planx.xmlstore.NodeFactory;

import dk.itu.bpl.dix.util.NodeUtil;

public class HighHole {
  private Node location;
  private Node parent;
  private int[] path;
  
  public HighHole(Node location, Node parent, int[] path) {
    if(location == null || parent == null || path == null)
      throw new IllegalArgumentException("Null is not allowed in argument");
    this.location = location;
    this.parent = parent;
    this.path = path;
  }
  
  public HighHole(Node location, Node parent, List<Node> usedChilds, int[] path) {
    if(location == null || parent == null || usedChilds == null || path == null) 
      throw new IllegalArgumentException("Null is not allowed in argument");
    List<Node> pChilds = new ArrayList<Node>();
    List<? extends Node> oldChilds = parent.getChildren();
    List<Node> tmpUsed = new ArrayList<Node>();
    tmpUsed.addAll(usedChilds);
    for(Node n : oldChilds) {
      boolean isUsed = false;
      for(int i = 0; i < tmpUsed.size() && !isUsed; i++) {
        if(n.contentEquals(tmpUsed.get(i))) {
          isUsed = true;
          tmpUsed.remove(i);
        } else {
          pChilds.add(n);
        }
      }
    }
    this.parent = NodeFactory.instance().createElementNode(parent.getNodeValue(), pChilds, parent.getAttributes());
    this.location = location;
    this.path = path;
   }
  
  public Node getLocation() {
    return location;
  }
  
  public Node getParent() {
    return parent;
  }
  
  public int[] getPath(){
    return path;
  }
  
  public boolean equals(Object o) {
    if(! (o instanceof HighHole))
      return false;
    HighHole h = (HighHole) o;
    if(h.path.length != path.length) return false;
    for(int i = 0; i<path.length; i++){
      if(path[i] != h.path[i]) return false;
    }
    return location.contentEquals(h.location) && parent.contentEquals(h.parent);
  }
  
}
