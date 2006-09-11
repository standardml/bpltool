package dk.itu.bpl.dix.reactivexml;

import java.util.*;

import org.planx.xmlstore.Node;
public class ContextHoleMatch {

  private WideMatch match;
  private Map<Node, HolePosition> pos;
  private List<? extends Node> context;

  public ContextHoleMatch(WideMatch match, Map<Node, HolePosition> pos, List<? extends Node> context) {
    this(match, pos);
    this.context = context;
  }
  
  public ContextHoleMatch(WideMatch match, Map<Node, HolePosition> pos, Node context) {
    this(match, pos);
    List<Node> con = new LinkedList<Node>();
    con.add(context);
    this.context = con;
  }
  
  private ContextHoleMatch(WideMatch match, Map<Node, HolePosition> pos) {
    this.match = match;
    this.pos = pos;    
  }

  public WideMatch getWideMatch() {
    return match;
  }
  
  public Map<Node, HolePosition> getPosition(){
    return pos;
  }
  
  public MatchStore getMatchStore(){
    return match.getMatchStore();
  }
  
  public HoleMap getHoleMap(){
    return match.getHoleMap();
  }
  
  public List<? extends Node> getContext(){
    return context;
  }
}
