package dk.itu.bpl.dix.reactivexml;

import org.apache.log4j.Logger;
import org.planx.xmlstore.*;
import org.planx.xpath.*;
import org.planx.xpath.object.*;
import dk.itu.bpl.dix.store.*;
import dk.itu.bpl.dix.util.*;

import java.io.IOException;
import java.util.*;

public class RewriteRuleSet {
  private static final Logger logger = Logger.getLogger(RewriteRuleSet.class);
  private Node rewriteRuleSet;

  private ArrayList<RewriteRule> rewriteRules;

  private String name;

  private String constraint;

  private Hashtable<String, List<CompleteWideMatch>> completeMatches;
  private ReactiveStore store;

  public RewriteRuleSet(Node root, ReactiveStore store)
      throws RewriteException, RuleException {
    rewriteRuleSet = root;
    name = root.getAttribute("NAME");
    constraint = root.getAttribute("CONSTRAINT");
    this.store = store;
    rewriteRules = new ArrayList<RewriteRule>();
    List<? extends Node> rewriteRuleNodes = root.getChildren();
    for (Node rule : rewriteRuleNodes) {
      rewriteRules.add(new RewriteRule(rule, this));
    }
    if ("".equals(constraint)) {

      constraint = "//*";
    }
  }

  private EvaluationContext getContext() throws RewriteException {
    try {
      return store.getEvaluationContext(new XPath(constraint,
          new XMLStoreNavigator()));
    } catch (IOException e) {
      throw new RewriteException("Could not get evaluation context", e);
    } catch (NameServerException e) {
      throw new RewriteException("Could not get evaluation context", e);
    } catch (XPathException e) {
      throw new RewriteException("Could not get evaluation context", e);
    }
  }

  public Hashtable<String, List<CompleteWideMatch>> findAllReactions()
      throws RewriteException {
    Hashtable<String, List<CompleteWideMatch>> result = 
      new Hashtable<String, List<CompleteWideMatch>>();
    EvaluationContext context = getContext();
    for (RewriteRule rwr : rewriteRules) {
      result.put(rwr.getName(), rwr.findReactions(context));
    }
    this.completeMatches = result;
    return result;
  }

  public void performReaction(String ruleName, int index)
      throws RewriteException, ConflictException {
    List<CompleteWideMatch> matchSet = completeMatches.get(ruleName);
    CompleteWideMatch toPerform = matchSet.get(index);
    try {
      store.reactWide(toPerform.getContext(), toPerform.createReactum());
    } catch (Exception e) {
      logger.debug("Got Exception", e);
      throw new RewriteException("Could not perform reaction", e);
    }
  }

  /**
   * @return Returns the rewirteRules.
   */
  public ArrayList getRewriteRules() {
    return rewriteRules;
  }

  /**
   * @return Returns the constraint.
   */
  public String getConstraint() {
    return constraint;
  }

  /**
   * @return Returns the name.
   */
  public String getName() {
    return name;
  }

  /**
   * @return Returns the rewriteRuleSet.
   */
  public Node getRewriteRuleSet() {
    return rewriteRuleSet;
  }
}
