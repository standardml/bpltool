package dk.itu.bpl.dix.reactivexml;

import java.util.*;

import org.apache.log4j.Logger;
import org.planx.xmlstore.*;

public class Match {

  private ArrayList<Node> nodesUsed;
  private MatchStore matchStore;
  private HoleMap holeMap;
  private ContextHoleMap contextMap;

  //Logger 
  private static final Logger logger = Logger.getLogger(Match.class);

  public ArrayList<Node> getNodesUsed() {
    return (ArrayList<Node>) nodesUsed.clone();
  }

  public HoleMap getHoleMap() {
    return holeMap;
  }

  public MatchStore getMatchStore() {
    return matchStore;
  }

  public Match(Node n, HoleMap h, MatchStore s, ContextHoleMap conMap) {
    nodesUsed = new ArrayList();
    nodesUsed.add(n);
    holeMap = h;
    matchStore = s;
    this.contextMap = conMap;
  }

  public Match(ArrayList<Node> nodesUsed, HoleMap h, MatchStore s,
      ContextHoleMap conMap) {
    this.nodesUsed = nodesUsed;
    holeMap = h;
    matchStore = s;
    this.contextMap = conMap;

  }

  public Match(HoleMap h, MatchStore s) {
    contextMap = new ContextHoleMap();
    holeMap = h;
    matchStore = s;
    nodesUsed = null;
  }

  public Match(HoleMap h, HighHoleMap hh, MatchStore s, ContextHoleMap conMap) {
    holeMap = h;
    matchStore = s;
    nodesUsed = null;
    this.contextMap = conMap;

  }

  public boolean usesNode(Node n) {
    logger.debug("Checking if node\n" + n + "\nis used\n------");
    if (nodesUsed == null) {
      logger.debug("No nodes used..");
      return false;
    }
    for (Node u : nodesUsed) {
      logger.debug("checking agains node:\n" + u);
      if (u.equals(n)) {
        logger.debug("Node equals");
        return true;
      }
    }
    return false;
  }

  public ContextHoleMap getContextHoleMap() {
    return contextMap;
  }

  public static Match join(Match m1, Match m2) throws HoleException {
    HoleMap h = HoleMap.join(m1.holeMap, m2.holeMap);
    MatchStore ms = MatchStore.join(m1.matchStore, m2.matchStore);
    ArrayList nu = new ArrayList();
    if (m1.nodesUsed != null) {
      for (Node n : m1.nodesUsed) {
        nu.add(n);
      }
    }
    if (m2.nodesUsed != null) {
      for (Node n : m2.nodesUsed) {
        nu.add(n);
      }
    }
    ContextHoleMap conMap = ContextHoleMap.join(m1.contextMap, m2.contextMap);
    return new Match(nu, h, ms, conMap);
  }

  public String toString() {
    StringBuffer buf = new StringBuffer();
    buf.append("Variables have values \n\n");
    Set<String> vars = matchStore.getVariables();
    for (String s : vars) {
      buf.append("\n" + s + " -> " + matchStore.getVariable(s));
    }
    buf.append("\n\n");
    buf.append("Nodes used in match --> \n");
    Iterator un = nodesUsed.iterator();
    while (un.hasNext()) {
      buf.append(un.next() + "\n\n");
    }
    buf.append("\n\n\n");
    return buf.toString();
  }
}