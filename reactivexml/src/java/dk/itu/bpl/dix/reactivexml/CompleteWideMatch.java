package dk.itu.bpl.dix.reactivexml;

import java.util.*;

import org.apache.log4j.Logger;
import org.planx.xmlstore.*;
import org.planx.xpath.*;
import org.planx.xpath.object.XNodeSet;
import dk.itu.bpl.dix.store.*;

import dk.itu.bpl.dix.util.NodeUtil;

public class CompleteWideMatch {
  private Hashtable<Node, Node> widesMap;
  private WideMatch wideMatch;
  private EvaluationContext context;

  private static final Logger logger = Logger
      .getLogger(CompleteWideMatch.class);

  public CompleteWideMatch(WideMatch wideMatch, Hashtable<Node, Node> widesMap,
      EvaluationContext context) {
    this.wideMatch = wideMatch;
    this.widesMap = widesMap;
    this.context = context;
  }

  public Hashtable<Node, Node> createReactum() throws RewriteException {
    Hashtable<Node, Node> result = new Hashtable<Node, Node>();
    logger.debug("No of primematches: " + wideMatch.getPrimeMatches().size());
    while (!wideMatch.getPrimeMatches().isEmpty()) {
      createReactum(wideMatch.getPrimeMatches(), result, false, 0);
    }
    return result;
  }

  public Node createReactum(List<PrimeMatch> remainingMatches,
      Hashtable<Node, Node> result, boolean isChild, int nextMatch)
      throws RewriteException {
    PrimeMatch m = remainingMatches.remove(nextMatch);
    logger.debug("Entering createReactum...");
    // logger.debug("Entering loop...");
    logger.debug("resterendeMatches.size() = " + remainingMatches.size());
    DocNode location = (DocNode) m.getLocation();
    List<Node> childsList = new ArrayList<Node>();
    List<? extends Node> rightChilds = widesMap.get(m.getWide()).getChildren();
    // Childs used in this reaction
    for (Node n : rightChilds) {
      childsList.addAll(createNodes(n));
    }
    List<Node> usedNodes = m.getMatch().getNodesUsed();
    List<DocNode> oldChilds = location.getDocChildren();
    int counter = 0;
    while (!remainingMatches.isEmpty() && counter < remainingMatches.size()) {
      PrimeMatch om = remainingMatches.get(counter);
      DocNode omLoc = (DocNode) om.getLocation();
      //logger.debug("omLoc is" + omLoc);
      //logger.debug("location is" + location);
      int pathIndex = NodeUtil.isChildOf(location, omLoc);
      //logger.debug("pathindexs is " + pathIndex);
      if (pathIndex > -2) {
        if (pathIndex == -1) {
          //logger.debug("Prime match is Sibling");
          List<? extends Node> sibChild = widesMap.get(om.getWide())
              .getChildren();
          List<Node> mUsed = new ArrayList<Node>();
          mUsed.addAll(usedNodes);
          // logger.debug("Used Nodes: \n" + mUsed);
          for (Node n : sibChild) {
            List<Node> rn = createNodes(n);
            for (Node nn : rn) {
              // logger.debug("Checking node: \n" +nn);
              if (!mUsed.remove(nn)) {
                // logger.debug("node is not used - adding it to childlist");
                childsList.add(nn);
                // usedNodes.add(nn);
              } else {
                // logger.debug("Node not used");
              }

            }
          }
          usedNodes.addAll(om.getMatch().getNodesUsed());
          remainingMatches.remove(counter);
        } else {
          // Is child of
          // logger.debug("PrimeMatch is Child of...");
          // logger.debug("Child path is: " + pathToString(omLoc.getPath()));
          // logger.debug("Path start index: " + pathIndex);
          int[] omLocPath = omLoc.getPath();
          boolean flag = true;
          for (int i = 0; i < oldChilds.size() && flag; i++) {
            DocNode n = oldChilds.get(i);
            if (n.getIndex() == omLocPath[pathIndex]) {
              childsList.add(NodeUtil.replace(n, createReactum(
                  remainingMatches, result, true, counter), omLocPath,
                  pathIndex + 1));
              usedNodes.add(n);
              flag = false;
            }
          }
        } 
      } else {
        //is not relative
        counter++;
      }
    }
    Set<Map.Entry<Node, Node>> resEntries = result.entrySet();
    List<Node> toBeRemoved = new ArrayList<Node>();
    for (Map.Entry<Node, Node> ent : resEntries) {
      DocNode oldLoc = (DocNode) ent.getKey();
      int pathIndex = NodeUtil.isChildOf(location, oldLoc);
      if (pathIndex > -2) {
        if (pathIndex == -1) {
          // logger.debug("TODOOOOO");
        }
        int[] oldLocPath = oldLoc.getPath();
        for (DocNode n : oldChilds) {
          if (n.getIndex() == oldLocPath[pathIndex]) {
            childsList.add(NodeUtil.replace(n, ent.getValue(), oldLocPath,
                pathIndex + 1));
            usedNodes.add(n);
          }
        }
        toBeRemoved.add(oldLoc);
      }
    }
    for (Node n : toBeRemoved) {
      result.remove(n);
    }
    for (Node n : oldChilds) {
      boolean nodeUsed = false;
      int i = 0;
      for (; i < usedNodes.size() && !nodeUsed; i++) {
        Node used = usedNodes.get(i);
        if (used.contentEquals(n)) {
          nodeUsed = true;
          usedNodes.remove(i);
        }
      }
      if (!nodeUsed) {
        childsList.add(n);
      }
    }
    List<Attribute> attributes = location.getAttributes();
    Node nLoc = NodeFactory.instance().createElementNode(
        location.getNodeValue(), childsList, attributes);

    if (!isChild)
      result.put(location, nLoc);
    return nLoc;
  }

  List<Node> createNodes(Node right) throws RewriteException {
    if ("RULE_HOLE".equals(right.getNodeValue())) {
      String holeName = right.getAttribute("NAME");
      return wideMatch.getHoleMap().getNodesMappedBy(holeName);
    }
    if ("CONTEXT_HOLE".equals(right.getNodeValue())) {
      ContextHoleMatch hole = wideMatch.getContextHoleMap().getContextHole(
          right.getAttribute("NAME"));
      List<? extends Node> regs = right.getChildren().get(0).getChildren();
      WideMatch wm = hole.getWideMatch();
      List<PrimeMatch> primeMatches = wm.getPrimeMatches();
      if(primeMatches.size() != regs.size()) {
        throw new RewriteException("The left and right width of context hole: '" 
            + right.getAttribute("NAME") + "' did not match");
      }
      Map<Node, HolePosition> posMap = hole.getPosition();
      //Map<Node, Node> widesMap = new Hashmap<Node, Node>();
      List<? extends Node> context = hole.getContext();
      List<Node> res = new ArrayList<Node>();
      for(int i = 0; i < primeMatches.size(); i++) {
        
        PrimeMatch pm = primeMatches.get(i);
        Node loc = pm.getLocation();
        //logger.debug("posMap is: " + posMap);
        HolePosition pos = posMap.get(loc);
        Node con = pos.getParent();
        context.remove(con);
        Node locNode = NodeUtil.getNode(con, pos.getPath());
        List<? extends Node> childs = con.getChildren();
        List<Node> newChilds = new ArrayList<Node>();
        List<Node> childsUsed = pm.getMatch().getNodesUsed();
        for(Node c : childs) {
          if(! childsUsed.remove(c)) {
            newChilds.add(c);
          }
        }
        newChilds.addAll(createNodes(regs.get(i)));
        locNode = NodeFactory.instance().createElementNode(locNode.getNodeValue(), 
            newChilds, locNode.getAttributes());
        
        res.add(NodeUtil.replace(con, locNode, pos.getPath(), 0));
      }
      res.addAll(context);
      return res;
    }

    if ("execXPath".equals(right.getNodeValue())) {
      String xpath = wideMatch.getMatchStore().getVariable(
          right.getAttribute("XPATH_VAR"));
      String xpathRootNode = right.getAttribute("XPATH_ROOT");
      List<? extends Node> childs = right.getChildren();
      if (childs.size() != 1) {
        throw new RewriteException(
            "An EXEC node must contain 1 child - no more, no less");
      }
      Node root = createNodes(childs.get(0)).get(0);
      try {
        XPath xp = new XPath(xpath, new XMLStoreNavigator());
        XNodeSet res = (XNodeSet) xp.evaluate(new DocNode(root));
        return res;
      } catch (XPathException e) {
        throw new RewriteException(e);
      }
    }

    List<Attribute> old = right.getAttributes();
    List<Attribute> newA = new LinkedList<Attribute>();
    for (Attribute a : old) {
      if (a.getValue().startsWith("$")) {
        newA.add(NodeFactory.instance().createAttribute(a.getName(),
            wideMatch.getMatchStore().getVariable(a.getValue())));
      } else {
        newA.add(a);
      }

    }
    List<? extends Node> oldChilds = right.getChildren();

    List<? extends Node> newChilds = new ArrayList<Node>();
    for (Node n : oldChilds) {
      List c = createNodes(n);
      if (c != null) {
        newChilds.addAll(c);
      }
    }
    Node newNode = NodeFactory.instance().createElementNode(
        right.getNodeValue(), newChilds, newA);
    List res = new ArrayList();
    res.add(newNode);
    return res;
  }

  public List<PrimeMatch> getPrimeMatches() {
    return wideMatch.getPrimeMatches();
  }

  public HoleMap getHoleMap() {
    return wideMatch.getHoleMap();
  }

  public MatchStore getMatchStore() {
    return wideMatch.getMatchStore();
  }

  public EvaluationContext getContext() {
    return context;
  }
}