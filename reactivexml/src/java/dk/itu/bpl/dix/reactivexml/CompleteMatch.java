package dk.itu.bpl.dix.reactivexml;

import java.util.*;

import org.apache.log4j.Logger;
import org.planx.xpath.*;
import org.planx.xmlstore.*;
import org.planx.xpath.object.*;


import dk.itu.bpl.dix.store.EvaluationContext;
import dk.itu.bpl.dix.util.NodeUtil;

public abstract class CompleteMatch {
  private static final Logger logger 
  = Logger.getLogger(CompleteMatch.class);
  public abstract Hashtable<Node, Node> createReactum() throws RewriteException;
  public abstract EvaluationContext getContext();
  public abstract List<Node> getLocations();
  public abstract Node getLocation();
  public abstract HoleMap getHoleMap();
  public abstract HighHoleMap getHighHoleMap();
  public abstract MatchStore getMatchStore();

  List<Node> createNodes(Node right) throws RewriteException{
    if ("RULE_HOLE".equals(right.getNodeValue())) {
      String holeName = right.getAttribute("NAME");
      return getHoleMap().getNodesMappedBy(holeName);
    }
    if("H_HOLE".equals(right.getNodeValue())) {
      HighHole hh = getHighHoleMap().getHole(right.getAttribute("NAME"));
      logger.debug("HighHole is: " + hh);
      Node loc = hh.getLocation();
      Node parent = hh.getParent();
      int[] path = hh.getPath();
      List<Node> newChilds = new ArrayList<Node>();
      for(Node n : right.getChildren()) {
        newChilds.addAll(createNodes(n));
      }
      newChilds.addAll(parent.getChildren());  
      Node newParent = NodeFactory.instance().createElementNode(parent.getNodeValue(), newChilds, parent.getAttributes());
      List<Node> res = new ArrayList<Node>();
      res.add(NodeUtil.replace(loc, newParent, path, 1));
      return res;
    }
    
    if("EXEC".equals(right.getNodeValue())) {
      String xpath = getMatchStore().getVariable(right.getAttribute("XPATH_VAR"));
      String xpathRootNode = right.getAttribute("XPATH_ROOT");
      logger.debug("xpath root: " + xpathRootNode);
      List<? extends Node> childs = right.getChildren();
      if(childs.size() != 1) {
        throw new RewriteException("An EXEC node must contain 1 child - no more, no less");
      }
      Node root = createNodes(childs.get(0)).get(0);
      try {
        logger.debug("Performing XPath: " + xpath);
        logger.debug("On root: " + root);
        XPath xp = new XPath(xpath, new XMLStoreNavigator());
        XNodeSet res = (XNodeSet) xp.evaluate(new DocNode(root));
        logger.debug("Returning: " + res);
        return res;
      } catch (XPathException e) {
        throw new RewriteException(e);
      }      
    }
    if("UPDATE".equals(right.getNodeValue())) {
      String xpath = getMatchStore().getVariable(right.getAttribute("XPATH_VAR"));
      Node xpathRoot = null;
      Node newNode = null;
      for(Node n : right.getChildren()) {
        if("ROOT".equals(n.getNodeValue())) {
            xpathRoot = createNodes(n.getChildren().get(0)).get(0);
        } else if("NEW_NODE".equals(n.getNodeValue())) {
          newNode = createNodes(n.getChildren().get(0)).get(0);
        }
      }
      if(xpathRoot == null || newNode == null || xpath == null) {
        throw new RewriteException("Illegal UPDATE node");
      }
      try{
        XPath xp = new XPath(xpath, new XMLStoreNavigator());
        XNodeSet xn = (XNodeSet) xp.evaluate(new DocNode(xpathRoot));
        DocNode dn = (DocNode) xn.get(0);
        int[] path = dn.getPath();
        List<Node> res = new ArrayList<Node>();
        res.add(NodeUtil.replace(xpathRoot, newNode, path, 1));
        return res;
      } catch (XPathException xe) {
        throw new RewriteException(xe);
      }
    }
    //create attributes
    List<Attribute> old = right.getAttributes();
    List<Attribute> newA = new LinkedList<Attribute>();
    for (Attribute a : old) {
      if (a.getValue().startsWith("$")) {
        logger.debug("Getting attrtibute: " + a.getValue());
        newA.add(NodeFactory.instance().createAttribute(a.getName(),
            getMatchStore().getVariable(a.getValue())));
        logger.debug("Got attribute");
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

}
