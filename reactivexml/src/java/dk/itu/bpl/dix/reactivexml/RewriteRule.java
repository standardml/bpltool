package dk.itu.bpl.dix.reactivexml;

import java.util.*;

import org.apache.log4j.Logger;
import org.planx.xmlstore.*;
import org.planx.xpath.XMLStoreNavigator;
import org.planx.xpath.XPath;
import org.planx.xpath.XPathException;
import org.planx.xpath.object.*;

import dk.itu.bpl.dix.store.EvaluationContext;
import dk.itu.bpl.dix.util.NodeUtil;

public class RewriteRule {
  private static final Logger logger = Logger.getLogger(RewriteRule.class);
  private String name;
  private String ruleConstraint;
  private RewriteRuleSet ruleSet;
  private List<Node> leftWides;
  private Hashtable<Node, Node> widesMap;

  public RewriteRule(Node rule, RewriteRuleSet ruleSet) throws RuleException {
    name = rule.getAttribute("NAME");
    ruleConstraint = rule.getAttribute("RULE_CONSTRAINT");
    this.ruleSet = ruleSet;
    widesMap = new Hashtable<Node, Node>();
    leftWides = new ArrayList<Node>();
    DocNode ruleDoc = new DocNode(rule);
    List<? extends Node> ruleLeftChilds = rule.getChildren().get(0)
        .getChildren().get(0).getChildren();
    List<? extends Node> ruleRightChilds = rule.getChildren().get(1)
        .getChildren().get(0).getChildren();
    for (int i = 0; i < ruleLeftChilds.size(); i++) {
      Node n = ruleLeftChilds.get(i);
      n = assureHoleOnWide(n, i);
      leftWides.add(n);
      widesMap.put(n, ruleRightChilds.get(i));
    }
  }

  public List<CompleteWideMatch> findReactions(EvaluationContext context) {
    List<? extends Node> contexts = context.getContextNodes();
    logger.debug("Entered findReactions size of leftWide:" + leftWides.size());
    List<WideMatch> matches = MatchFunctions.getCompleteWideMatch(leftWides,
        new WideMatch(), contexts, ruleSet.getConstraint());
    List<CompleteWideMatch> res = new ArrayList<CompleteWideMatch>();
    for (WideMatch wm : matches) {
      logger.debug("adding match - no of primematches: "
          + wm.getPrimeMatches().size());
      CompleteWideMatch cwm = new CompleteWideMatch(wm, widesMap, context);
      res.add(cwm);
    }
    return res;
  }

  public RewriteRuleSet getRewriteRuleSet() {
    return ruleSet;
  }

  public String getRuleConstraint() {
    return ruleConstraint;
  }

  List<DocNode> getSortedContextNodes(List<? extends Node> conNodes) {
    List<DocNode> contextNodes = new ArrayList<DocNode>();
    for (Node n : conNodes) {
      contextNodes.add((DocNode) n);
    }
    logger.debug("theres a constraint: " + ruleConstraint);
    //Sort by path
    Comparator<DocNode> dnComp = new Comparator<DocNode>() {
      public int compare(DocNode n1, DocNode n2) {
        int[] path1 = n1.getPath();
        int[] path2 = n2.getPath();
        if (path1.length > path2.length) {
          return 1;
        }
        if (path1.length < path2.length) {
          return -1;
        }
        return 0;
      }
    };
    Collections.sort(contextNodes, dnComp);
    return contextNodes;
  }

  List<List<? extends Node>> getContextSets(List<? extends Node> contexts,
      String constraint) {
    List<List<? extends Node>> res = new ArrayList<List<? extends Node>>();
    List<DocNode> contextNodes = getSortedContextNodes(contexts);
    try {
      XPath xp = new XPath(constraint, new XMLStoreNavigator());
      while (!contextNodes.isEmpty()) {
        DocNode dn = contextNodes.get(0);
        contextNodes.remove(0);
        XBoolean ok = (XBoolean) xp.evaluate(new DocNode(dn));
        if (ok.booleanValue()) {
          logger.debug("Got Matching context: \n" + dn.getNodeValue());
          List<Node> conNodes = new ArrayList<Node>();
          Iterator<DocNode> it = contextNodes.iterator();
          conNodes.add(dn);
          logger.debug("Checking for childs..");
          while (it.hasNext()) {
            DocNode ch = it.next();
            if (NodeUtil.isChildOf(dn, ch) > -1) {
              logger.debug("Found child: \n" + ch.getNodeValue());
              conNodes.add(ch);
              it.remove();
            }
          }
          res.add(conNodes);
        }
      }
    } catch (XPathException xe) {
    }
    return res;
  }

  List<List<? extends Node>> getContextNodes(EvaluationContext context) {
    logger.debug("Getting context nodes");
    if (ruleConstraint == null || "".equals(ruleConstraint)) {
      List<List<? extends Node>> res = new ArrayList<List<? extends Node>>();
      logger.debug("No rule constraint - constraint is:" + ruleConstraint);
      res.add(context.getContextNodes());
      return res;
    } else {
      return getContextSets(context.getContextNodes(), ruleConstraint);
    }
  }

  Node assureHoleOnWide(Node n, int no) {
    List<Node> childs = new ArrayList<Node>();
    childs.addAll(n.getChildren());
    if (MatchFunctions.getHoleChild(n) == null) {
      List<Attribute> attr = new ArrayList<Attribute>();
      attr.add(NodeFactory.instance()
          .createAttribute("NAME", "ROOT_CHILD_HOLE"));
      childs.add(NodeFactory.instance().createElementNode("RULE_HOLE",
          new ArrayList<Node>(), attr));
    }
    List<Attribute> attr = new ArrayList<Attribute>();
    attr.add(NodeFactory.instance().createAttribute("NO", "" + no));
    return NodeFactory.instance().createElementNode(n.getNodeValue(), childs,
        attr);

  }

  Node assureRootHole(Node n) {
    if (MatchFunctions.getHoleChild(n) == null) {
      List<Attribute> attr = new ArrayList<Attribute>();
      attr.add(NodeFactory.instance()
          .createAttribute("NAME", "ROOT_CHILD_HOLE"));
      Node hole = NodeFactory.instance().createElementNode("RULE_HOLE",
          new ArrayList<Node>(), attr);
      return NodeFactory.instance()
          .insertChild(n, n.getChildren().size(), hole);
    }
    return n;
  }

  public String getName() {
    return name;
  }
}
