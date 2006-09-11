package dk.itu.bpl.dix.reactivexml;

import org.apache.log4j.Logger;
import org.planx.xmlstore.*;
import org.planx.xpath.XMLStoreNavigator;
import org.planx.xpath.XPath;
import org.planx.xpath.XPathException;
import org.planx.xpath.object.*;

import dk.itu.bpl.dix.store.EvaluationContext;
import dk.itu.bpl.dix.util.*;

import java.util.*;

public class MatchFunctions {

  private static final Logger logger = Logger.getLogger(MatchFunctions.class);

  /**
   * Gets all possible set of matches for a wide rule
   * 
   * @param remainingWides a list of non matched wides
   * @param thisMatch Prvously found match
   * @param contextNodes list of context node 
   * @param constraint rule constraint
   * @return a List of WideMatches
   */
  protected static List<WideMatch> getCompleteWideMatch(
      List<? extends Node> remainingWides, WideMatch thisMatch,
      List<? extends Node> contextNodes, String constraint) {
    if (remainingWides.isEmpty()) {
      return null;
    }
    logger.debug("Looking for wide match....");
    logger.debug("reaainingwides is: \n" + remainingWides);
    logger.debug("Context Nodes is: " + contextNodes);
    List<WideMatch> res = new ArrayList<WideMatch>();
    for (Node loc : contextNodes) {
      List<WideMatch> matches = getWideMatches(remainingWides, thisMatch, loc,
          constraint);
      // For each of the matches try matching with the other contexts
      for (WideMatch wm : matches) {
        List<Node> newRemainingWides = new ArrayList<Node>();
        List<Node> usedWides = wm.getWides();
        for (Node wide : remainingWides) {
          if (!usedWides.remove(wide)) {
            newRemainingWides.add(wide);
          }
        }
        ArrayList<Node> newContext = new ArrayList<Node>();
        for (Node n : contextNodes) {
          if (!n.equals(loc))
            newContext.add(n);
        }
        List<WideMatch> moreMatches = getCompleteWideMatch(newRemainingWides,
            wm, newContext, constraint);
        if (moreMatches == null) {
          res.add(wm);
        } else {
          res.addAll(moreMatches);
        }
      }
    }
    return res;
  }

  /**
   * Finds all possible matches for the list of wides in this context
   */
  private static List<WideMatch> getWideMatches(List<? extends Node> wides,
      WideMatch match, Node location, String constraint) {
    List<WideMatch> res = new ArrayList<WideMatch>();
    for (int i = 0; i < wides.size(); i++) {
      List<WideMatch> matches = getWideMatch(0, i, wides, location, match,
          location, constraint);
      if (matches != null) {
        res.addAll(matches);
      }
    }
    return res;
  }

  private static List<WideMatch> getWideMatch(int firstIndex, int lastIndex,
      List<? extends Node> wides, Node location, WideMatch thisMatch,
      Node newLoc, String constraint) {
    if (firstIndex > lastIndex) {
      return null;// thisMatch;
    }
    List<WideMatch> res = new ArrayList<WideMatch>();
    Node wide = wides.get(firstIndex);
    List<? extends Match> singleMatch = MatchFunctions.findReactions(wide,
        newLoc, thisMatch.getMatchStore(), constraint);
    if (singleMatch != null) {
      for (Match m : singleMatch) {
        PrimeMatch pm = new PrimeMatch(m, location, wide);
        try {
          WideMatch newMatch = WideMatch.join(thisMatch,
              new WideMatch(wide, pm));
          List<? extends Node> oldLocChilds = newLoc.getChildren();
          List<Node> newLocChilds = new ArrayList<Node>();
          List<Node> usedNodes = m.getNodesUsed();
          for (Node oc : oldLocChilds) {
            boolean used = false;
            for (int i = 0; i < usedNodes.size() && !used; i++) {
              Node un = usedNodes.get(i);
              if (un.equals(oc)) {
                used = true;
                usedNodes.remove(i);
              }
            }
            if (!used)
              newLocChilds.add(oc);
          }
          newLoc = NodeFactory.instance().createElementNode(
              newLoc.getNodeValue(), newLocChilds, newLoc.getAttributes());
          List<WideMatch> moreMatches = getWideMatch(firstIndex + 1, lastIndex,
              wides, location, newMatch, newLoc, constraint);

          if (moreMatches == null) {
            res.add(newMatch);
          } else {
            res.addAll(moreMatches);
          }
        } catch (HoleException he) {
          // Do nothing - the two matches could not be joined.
        }
      }
    } else {
    }
    return res;
  }

  /**
   * 
   * Gets all the possible context hole match, for each match in matches. 
   * 
   * @param matches
   * @param hole
   * @param s
   * @param n
   * @param constraint
   * @return
   */
  private static ArrayList<Match> getContextHoleMatch(List<Match> matches,
      Node hole, MatchStore s, Node n, String constraint) {
    ArrayList<Match> r = new ArrayList<Match>();
    for (Match m : matches) {
      ArrayList<Node> nonUsed = new ArrayList<Node>();
      List<? extends Node> nChilds = n.getChildren();
      for (Node node : nChilds) {
        if (!m.usesNode(node)) {
          nonUsed.add(node);
        }
      }
      List<ContextHoleMatch> conMatch = new LinkedList<ContextHoleMatch>();
      for(Node noUse :nonUsed) {
        conMatch.addAll(getContextHoleMatch(hole, noUse, s, constraint));
      }
      
      //List<ContextHoleMatch> conMatch = getContextHoleMatch(hole, nonUsed, s,
      //    constraint);
      for(ContextHoleMatch chm : conMatch) {
        MatchStore store = MatchStore.join(chm.getMatchStore(), m.getMatchStore());
        ContextHoleMap thisMap = new ContextHoleMap(hole.getAttribute("NAME"), chm);
        thisMap = ContextHoleMap.join(thisMap, m.getContextHoleMap());
        try {
          HoleMap hMap = HoleMap.join(chm.getHoleMap(), m.getHoleMap());
          r.add(new Match(m.getNodesUsed(), hMap, store, thisMap));
        } catch (HoleException he) {          
        }
      }
    }
    return r;
  }

  private static List<ContextHoleMatch> getContextHoleMatch(Node hole,
      Node pToMatch, MatchStore store, String constraint) {
    logger.debug("getContextHole");
    logger.debug("Matching hole against: \n" + pToMatch);
    String xp = hole.getAttribute("XPATH");
    List<? extends Node> holeExpr = hole.getChildren().get(0).getChildren();
    List<Node> conNodes = new ArrayList<Node>();
    Map<Node, HolePosition> posMap = new HashMap<Node, HolePosition>();
    if ("".equals(xp) || xp == null) {
      logger.debug("Context is constraint: " + constraint);
      try {
        XPath xpath = new XPath(constraint, new XMLStoreNavigator());
        List<DocNode> xpRes = (List<DocNode>) xpath.evaluate(new DocNode(pToMatch));
        for(DocNode xpn : xpRes) {
          conNodes.add(xpn);
          posMap.put(xpn, new HolePosition(pToMatch, xpn.getPath()));
        }
        conNodes.addAll(xpRes);
        /*
        for (Node n : pToMatch) {
          //n = NodeFactory.instance().createElementNode("reg", n);
          List<DocNode> xpRes = (List<DocNode>) xpath.evaluate(new DocNode(n));
          for(DocNode xpn : xpRes) {
            conNodes.add(xpn);
            posMap.put(xpn, new HolePosition(n, xpn.getPath()));
          }
          conNodes.addAll(xpRes);
          //Hack...
          //conNodes.add(n);
        }
        */
      } catch (XPathException xe) {
        logger.debug("Got XPath exception");
      }
    } else {
      //List<Node> xpRes = new ArrayList<Node>();
      List<Node> rootChilds = new ArrayList<Node>();
      try {
        XPath xpath = new XPath(xp, new XMLStoreNavigator());
        //for (Node n : pToMatch) {
          XNodeSet xns = (XNodeSet) xpath.evaluate(new DocNode(pToMatch));
          while(!xns.isEmpty()) {
            DocNode first = (DocNode) xns.remove(0);
            if(first.getPath().length < 1) {
              //This is a root node 
              rootChilds.add(first);
            } else {
              //It is not a root child and we look for siblings
              List<Node> siblings = new ArrayList<Node>();
              siblings.add(first);
              for(int i = 1; i < xns.size(); i++) {
                int pathIndex = NodeUtil.isChildOf(first, (DocNode)xns.get(i));
                if(pathIndex > -2) {
                  if (pathIndex > -1) {
                    // TODO throw exception - the node is child to this one....
                  }
                  siblings.add((Node) xns.get(i));
                  xns.remove(i);
                }
              }
              Node reg = NodeFactory.instance().createElementNode("reg", siblings);
              posMap.put(reg, new HolePosition(pToMatch, first.getParent().getPath()));
              conNodes.add(reg);              
            }
          }
        //}
      } catch (XPathException xe) {
      }
      conNodes.add(NodeFactory.instance().createElementNode("reg", rootChilds));
    }
    logger.debug("locations nodes:\n" + conNodes);
    List<ContextHoleMatch> res = new ArrayList<ContextHoleMatch>();
    List<WideMatch> matches = getCompleteWideMatch(holeExpr, new WideMatch(store), 
        conNodes, constraint);
    logger.debug("getCompleteWideMatch returned "  + matches.size() +" matches");
    for(WideMatch match : matches) {
      res.add(new ContextHoleMatch(match, posMap, pToMatch));
    }
    return res;    
  }

  public static MatchStore compareNodes(Node t, Node n, MatchStore s) {
    if (!t.getNodeValue().equals(n.getNodeValue())) {
      return null;
    }
    if (t.getAttributes().size() != n.getAttributes().size()) {
      return null;
    }
    MatchStore newM;
    newM = s.copy();
    List<Attribute> tattr = t.getAttributes();
    for (Attribute a : tattr) {
      String aValue = n.getAttribute(a.getName());
      if (aValue == null) {
        // The node n dosen't have the attribute
        return null;
      }
      if (!a.getValue().startsWith("$")) {
        // Its not a Variable and has to equals aValue
        if (!a.getValue().equals(aValue)) {
          return null;
        }
      } else {
        if (newM.hasVariable(a.getValue())) {
          // The variable is already in the store
          if (!newM.getVariable(a.getValue()).equals(aValue)) {
            // But dosen't match
            return null;
          }
        } else {
          // The variable is not in the store - so let's add it
          newM.addVariable(a.getValue(), aValue);
        }

      }

    }
    return newM;
  }

  public static ArrayList findCompleteMatches(List<Node> tChilds,
      List<? extends Node> nChilds, MatchStore s, String constraint) {
    ArrayList p = new ArrayList();
    Iterator it = tChilds.iterator();
    while (it.hasNext()) {
      ArrayList tmp = new ArrayList();
      Node t = (Node) it.next();
      for (Node n : nChilds) {
        ArrayList<? extends Match> tmpRes = MatchFunctions.findReactions(t, n,
            s, constraint);
        if (tmpRes != null) {
          for (Match m : tmpRes) {
            tmp.add(new Match(n, m.getHoleMap(), m.getMatchStore(), m
                .getContextHoleMap()));
          }
        }
      }
      if (tmp.size() == 0) {
        return null;
      }
      p.add(tmp);
    }
    ArrayList m = (ArrayList) p.get(0);
    logger.debug("m: " +  m);    
    for (int i = 1; i < p.size(); i++) {
      if (m != null && m.size() != 0) {
        ArrayList am = (ArrayList) p.get(i);
        logger.debug("am: " + am);
        m = MatchFunctions.joinMatches(m, am);
      } else {
        return null;
      }
    }
    return m;
  }

  public static ArrayList joinMatches(ArrayList<Match> match,
      ArrayList<Match> pmatch) {
    logger.debug("Join matches called....");
    logger.debug("Arg1: \n" + match);
    logger.debug("Arg2: \n" + pmatch);
    ArrayList newM = new ArrayList();
    for (Match m : match) {
      for (Match p : pmatch) {
        if (p.getNodesUsed() == null || p.getNodesUsed().size() > 1)
          return null;
        Node m2Uses = p.getNodesUsed().get(0);
        if (!m.usesNode(m2Uses)) {
          boolean varsOk = true;
          Set<String> pVariables = p.getMatchStore().getVariables();
          for (String var : pVariables) {
            if (m.getMatchStore().hasVariable(var)) {
              if (!m.getMatchStore().getVariable(var).equals(
                  p.getMatchStore().getVariable(var))) {
                varsOk = false;
                break;
              }
            }
          }
          if (varsOk) {
            try {
              logger.debug("m: " + m);
              logger.debug("p: " + p);
              Match newMatch = Match.join(m, p);
              newM.add(newMatch);
            } catch (HoleException e) {
              logger.debug("joinMatches - returning null");
              return null;
            }
          }
        }
      }
    }
    logger.debug("joinMatches returning with result");
    return newM;
  }

  public static ArrayList<Match> mapHoles(ArrayList<Match> matchs, String s,
      Node n) throws HoleException {
    // logger.debug("mapsHoles called....");
    if ("ROOT_CHILD_HOLE".equals(s))
      return matchs;
    ArrayList<Match> r = new ArrayList<Match>();
    for (Match m : matchs) {
      HoleMap newMap = m.getHoleMap();
      ArrayList mapsTo = new ArrayList();
      List<? extends Node> nChilds = n.getChildren();
      for (Node node : nChilds) {
        if (!m.usesNode(node)) {
          mapsTo.add(node);
        }
      }
      newMap = HoleMap.join(m.getHoleMap(), new HoleMap(s, mapsTo));
      r.add(new Match(m.getNodesUsed(), newMap, m.getMatchStore(), m
          .getContextHoleMap()));
    }
    return r;
  }

  public static ArrayList<Match> findReactions(Node t, Node n, MatchStore s,
      String constraint) {
    if (!"reg".equals(t.getNodeValue())) {
      s = MatchFunctions.compareNodes(t, n, s);
    }
    if (s == null) {
      return null;
    }
    int tChildren = t.getChildren().size();
    int nChildren = n.getChildren().size();
    if (tChildren == 0 && nChildren == 0) {
      ArrayList returnVal = new ArrayList();
      returnVal.add(new Match(new HoleMap(), s));
      return returnVal;
    }
    //Context Hole
    Node cHoleChild = MatchFunctions.getContextHoleChild(t);
    if (cHoleChild != null) {
      if (tChildren == 1) {
        
        List<ContextHoleMatch> matches = new LinkedList<ContextHoleMatch>();
        //TODO probably collect n children under one node...
        for(Node nodeToMatch : n.getChildren()) {
          matches.addAll(getContextHoleMatch(cHoleChild, nodeToMatch
            , s, constraint));
        }
//        List<ContextHoleMatch> matches = getContextHoleMatch(cHoleChild, n.getChildren()
//            , s, constraint);
        if (matches.isEmpty())
          return null;
        ArrayList<Node> used = new ArrayList<Node>();
        ArrayList<Match> res = new ArrayList<Match>();
        used.addAll(n.getChildren());
        for (ContextHoleMatch m : matches) {
          res.add(new Match(used, m.getHoleMap(), m.getMatchStore(),
              new ContextHoleMap(cHoleChild.getAttribute("NAME"), m)));
        }
        return res;
      }

      List<Match> m = MatchFunctions.findCompleteMatches(getNonHoleChildren(t),
          n.getChildren(), s, constraint);
      if (m == null)
        return null;
      ArrayList res = getContextHoleMatch(m, cHoleChild, s, n, constraint);
      if (res.isEmpty())
        return null;
      return res;
    }

    Node tHoleChild = MatchFunctions.getHoleChild(t);
    if ((tHoleChild == null && (tChildren != nChildren))
        || (tHoleChild != null && (tChildren - 1 > nChildren))) {
      return null;
    }

    // If t only have one child which is a hole
    // We match all nChildren to this hole.
    if (tHoleChild != null && tChildren == 1) {
      String holeName = tHoleChild.getAttribute("NAME");
      if (holeName == null) {
        return null;
      }
      HoleMap newMap = new HoleMap(holeName, n.getChildren());
      ArrayList returnVal = new ArrayList();
      // TODO because of type declaration in PMatch it has to be an ArrayList.
      // changes this in PMatch (its the use of clone, which demands an specific
      // list type)
      ArrayList<Node> used = new ArrayList<Node>(n.getChildren());
      returnVal.add(new Match(used, newMap, s, new ContextHoleMap()));
      return returnVal;
    }

    ArrayList m = MatchFunctions.findCompleteMatches(getNonHoleChildren(t), n
        .getChildren(), s, constraint);
    if (m == null)
      return null;
    if (tHoleChild != null) {
      try {
        return MatchFunctions.mapHoles(m, tHoleChild.getAttribute("NAME"), n);
      } catch (HoleException e) {
        return null;
      }
    }
    return m;
  }

  public static Node getContextHoleChild(Node n) {
    List<? extends Node> childs = n.getChildren();
    for (Node cn : childs) {
      if ("CONTEXT_HOLE".equals(cn.getNodeValue())) {
        // logger.debug("Returning: " + cn);
        return cn;
      }
    }
    // logger.debug("Returning null");
    return null;

  }

  public static Node getHoleChild(Node n) {
    List<? extends Node> childs = n.getChildren();
    for (Node cn : childs) {
      if ("RULE_HOLE".equals(cn.getNodeValue())) {
        return cn;
      }
    }
    return null;
  }

  public static List<Node> getNonHoleChildren(Node n) {
    List<Node> res = new ArrayList<Node>();
    for (Node cn : n.getChildren()) {
      if (!("RULE_HOLE".equals(cn.getNodeValue()) || "CONTEXT_HOLE".equals(cn
          .getNodeValue()))) {
        res.add(cn);
      }
    }
    return res;
  }
}
