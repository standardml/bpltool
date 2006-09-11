package dk.itu.bpl.dix.store;

import org.apache.log4j.Logger;
import org.planx.xmlstore.input.*;
import org.planx.xmlstore.*;

import java.util.*;
import org.planx.xpath.*;

import java.io.*;

import dk.itu.bpl.dix.util.*;

/**
 * @author Martin Olsen
 * @version $Revision: 1.10 $
 */
public class DistributedReactiveStore<R extends Reference> implements
    ReactiveStore {
  private XMLStore<R>         store;
  private NameServer<R>       ns;
  private XMLStoreNavigator   nav    = new XMLStoreNavigator();
  private static final Logger logger = Logger.getLogger(DistributedReactiveStore.class);

  public DistributedReactiveStore(XMLStore<R> store, NameServer<R> ns) {
    this.store = store;
    this.ns = ns;
  }

  public void init(String processTreeXML) throws NameAlreadyBoundException,
      IOException, NameServerException {
    Node r;
    try {
      r = SAXBuilder.build(processTreeXML);
    } catch (XMLException e) {
      IOException io = new IOException("XML Parse Error");
      io.initCause(e);
      throw io;
    }
    Node chgSet = NodeFactory.instance().createElementNode("ChangeSet");
    Node version = NodeFactory.instance().createElementNode("Version", new Node[] { r, chgSet });
    Node oldR = NodeFactory.instance().createElementNode("Root");
    Node root = NodeFactory.instance().createElementNode("Root", new Node[] { oldR, version });
    ns.bind("ReactiveXML", store.save(root));
  }

  public EvaluationContext getEvaluationContext(XPath xpath) throws NameServerException, IOException {
    Node root = store.load(ns.lookup("ReactiveXML"));
    return new EvaluationContext(xpath, root);
  }

  /**
   * This method performs a Wide reaction.
   * <b>WARNING: This method is not yet synchronized.
   * All width will be performed as a single reaction
   * hence if any other reaction is performed in the time
   * between two width performed, it might no be possible 
   * to perform all width.
   * </b>
   * @param context The evaluation context
   * @param width A map from each width to its reactum
   * @throws NameServerException 
   * @throws XPathException 
   * @throws ConflictException 
   * @throws IOException 
   */
  public void reactWide(EvaluationContext context, Map<Node, Node> width) throws IOException, ConflictException, XPathException, NameServerException {
    logger.debug("Entering wideReact");
    Set<Node> matches = width.keySet();
    for (Node match : matches) {
      react(context, match, width.get(match));
    }
  }

  public void react(EvaluationContext context, Node match, Node reactum) throws IOException, ConflictException, XPathException, NameServerException {
    Node root = context.getRoot();
    Node graph = NodeUtil.getNode("/Root/Version/child::*[1]", root);
    List<DocNode> contNode = (List<DocNode>) context.getContextNodes();
    //We are looking for the match in the contexts to get it's path
    DocNode mt = null;
    for (DocNode n : contNode) {
      if (match.contentEquals(n)) {
        mt = n;
      }
    }
    int[] path = mt.getPath();
    //Debugging
    StringBuffer buf = new StringBuffer();
    for (int i = 1; i < path.length; i++) {
      buf.append(path[i]);
      buf.append(":");
    }
    logger.debug("Path to match: " + buf.toString());
    logger.debug("Xpath to match: " + pathToXPath(path, 1));
    logger.debug("Match:\n" + mt);
    logger.debug("Graph:\n" + graph);
    logger.debug("Reactum:\n" + reactum);
    //logger.debug("old graph:\n" + graph);
    //End debug
    graph = NodeUtil.replace(graph, reactum, path, 1);
    logger.debug("new graph:\n" + graph);
    Attribute xpathAttr = NodeFactory.instance().createAttribute("value", pathToXPath(path, 1));
    List<Attribute> xpathAttrList = new ArrayList<Attribute>(1);
    xpathAttrList.add(xpathAttr);
    Node xpath = NodeFactory.instance().createElementNode("XPath", new ArrayList<Node>(), xpathAttrList);
    List<Node> chgChilds = new ArrayList<Node>(3);
    chgChilds.add(match);
    chgChilds.add(reactum);
    chgChilds.add(xpath);
    Node chgSet = NodeFactory.instance().createElementNode("ChangeSet", chgChilds);
    List<Node> vchilds = new ArrayList<Node>(2);
    vchilds.add(graph);
    vchilds.add(chgSet);
    Node version = NodeFactory.instance().createElementNode("Version", vchilds);
    List<Node> rootCh = new ArrayList<Node>(2);
    rootCh.add(root);
    rootCh.add(version);
    Node newRoot = NodeFactory.instance().createElementNode("Root", rootCh);
    commit(context.getRoot(), newRoot);
  }

  public Node getProcessTree() throws StoreNotInitializedException, IOException {
    try {
      R ref = ns.lookup("ReactiveXML");
      if (ref == null) {
        throw new StoreNotInitializedException();
      }
      return NodeUtil.getNode("/Root/Version/child::*[name()='wide']", store.load(ref));
    } catch (NameServerException ne) {
      throw new StoreNotInitializedException();
    }
  }

  public Node getRoot() throws Exception {
    if (ns.lookup("ReactiveXML") == null)
      throw new StoreNotInitializedException();
    return store.load(ns.lookup("ReactiveXML"));
  }

  private void commit(Node oldRoot, Node root) throws IOException, ConflictException, NameServerException {
    try {
      R oldRef = store.save(oldRoot);
      ns.rebind("ReactiveXML", oldRef, store.save(root));
    } catch (StaleReferenceException e) {
      Node newestRoot = store.load(ns.lookup("ReactiveXML"));
      Stack<Node> changeSets = new Stack<Node>();
      getChgSets(newestRoot, oldRoot, changeSets);
      execChgSets(changeSets, root, newestRoot);
    }
  }

  private void getChgSets(Node newRt, Node oldRoot, Stack<Node> res) {
    logger.debug("....getChgSets...");
    logger.debug("old root\n" + oldRoot);
    logger.debug("new root :\n" + newRt);
    if (!oldRoot.contentEquals(newRt)) {
      logger.debug("old root and new root did not content equals");
      res.push(NodeUtil.getNode("/Root/Version/ChangeSet", newRt));
      getChgSets(NodeUtil.getNode("/Root/child::*[1]", newRt), oldRoot, res);
    }
  }

  private void execChgSets(Stack<Node> chg, Node oldRoot, Node newRoot)
      throws ConflictException, IOException, NameServerException {
    Node version = NodeUtil.getNode("/Root/Version", oldRoot);
    while (!chg.empty()) {
      version = execChgSet(chg.pop(), version);
    }
    List<Node> childs = new ArrayList<Node>(2);
    childs.add(newRoot);
    childs.add(version);
    Node root = NodeFactory.instance().createElementNode("Root", childs);
    commit(newRoot, root);
  }

  private Node execChgSet(Node chgSet, Node version) throws IOException, ConflictException {
    logger.debug("Version node is\n" + version);
    Node r = NodeUtil.getNode("/Version/child::*[1]", version);
    logger.debug("r node is:" + r);
    String xpathNew = "/wide" + NodeUtil.getNode("/ChangeSet/XPath", chgSet).getAttribute("value");
    //aMatch on the path in this graph - we need to check
    //whether this corresponds to the match Node in the chgSet
    //logger.debug("Changeset : \n" + chgSet);
    logger.debug("XPath to match: " + xpathNew);
    Node aMatch = NodeUtil.getNode(xpathNew, r);
    logger.debug("a match :" + aMatch);
    Node chgMatch = NodeUtil.getNode("/ChangeSet/child::*[1]", chgSet);
    logger.debug("chgMatch" + chgMatch);
    if (!chgMatch.contentEquals(aMatch)) {
      throw new ConflictException();
    }

    Node ourMatch = NodeUtil.getNode("/Version/ChangeSet/child::*[1]", version);
    //check if we are trying to merge two incompatible changes
    if (inSubtree(ourMatch, chgMatch) | inSubtree(chgMatch, ourMatch))
      throw new ConflictException();
    //If we are this far we can safely update with changes from this chgSet
    Node change = NodeUtil.getNode("/ChangeSet/child::*[2]", chgSet);
    int[] path = xpathToIntArr(xpathNew);
    r = NodeUtil.replace(r, change, path, 0);
    return NodeFactory.instance().replaceChild(version, 0, r);
  }

  private int[] xpathToIntArr(String xpath) {
    String[] p = xpath.split("\\D*\\D");
    //The first thing in the array will be ""
    //so will start of at second index
    int[] path = new int[p.length - 1];
    for (int i = 1; i < p.length; i++) {
      path[i - 1] = Integer.parseInt(p[i]) - 1;
    }
    return path;
  }

  private boolean inSubtree(Node root, Node x) {
    if (root.contentEquals(x))
      return true;
    List<? extends Node> childs = root.getChildren();
    if (childs.size() == 0)
      return false;
    boolean res = false;
    for (int i = 0; i < childs.size() && !res; i++) {
      res = inSubtree(childs.get(i), x);
    }
    return res;
  }

  private String pathToXPath(int[] path, int offset) {
    StringBuffer buf = new StringBuffer();
    if (path.length == 0) {
      buf.append("self::*");
    }
    for (int i = offset; i < path.length; i++) {
      buf.append("/child::*[");
      buf.append(path[i] + 1);
      buf.append("]");
    }
    return buf.toString();
  }
}
