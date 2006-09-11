package dk.itu.bpl.dix.reactivexml;

import org.planx.xmlstore.*;

public class HolePosition {
  
  private Node parent;
  private int[] path;

  public HolePosition(Node parent, int[] path) {
    this.parent = parent;
    this.path = path;
  }
  
  public int[] getPath() {
    return path;
  }
  
  public Node getParent(){
    return parent;
  }

}
