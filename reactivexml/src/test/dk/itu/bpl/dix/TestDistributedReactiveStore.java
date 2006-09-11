package dk.itu.bpl.dix;

import junit.framework.TestCase;
import junit.textui.TestRunner;
import org.planx.xmlstore.koala.nameserver.*;
import org.planx.xmlstore.stores.*;
import org.planx.xmlstore.*;
import dk.itu.bpl.dix.store.*;
import java.io.File;
import java.util.*;
import org.planx.xpath.*;

/**
 * @author Martin Olsen
 * @version $Revision: 1.1 $
 */
public class TestDistributedReactiveStore extends TestCase {
  
  public void testReact() throws Exception {
    LocalXMLStore store = new LocalXMLStore("testStore");
    NameServer ns = new LocalNameServer(store);

    DistributedReactiveStore drs = new DistributedReactiveStore(store, ns);
    //First we initiate the store
    Node firstChgSet = NodeFactory.instance().createElementNode("ChangeSet");
    Node firstr = NodeFactory.instance().createElementNode("r");
    List<Node> firstVChilds = new ArrayList<Node>();
    firstVChilds.add(firstr);
    firstVChilds.add(firstChgSet);
    Node oldRoot = NodeFactory.instance().createElementNode("Root");
    Node firstVersion = NodeFactory.instance().createElementNode("Version", firstVChilds);
    List<Node> rch = new ArrayList<Node>();
    rch.add(oldRoot);
    rch.add(firstVersion);
    Node firstRoot = NodeFactory.instance().createElementNode("Root", rch);
    ns.bind("ReactiveXML", store.save(firstRoot));
    
    //Now we try to get the the context - which should just be the root
    XPath xpath = new XPath("self::*", new XMLStoreNavigator());
    EvaluationContext con = drs.getEvaluationContext(xpath);
    //There should be one node in the context which equals firstRoot
    List contextNodes = con.getContextNodes();
    Node expectedFirstRoot = (Node) contextNodes.get(0);
    assertEquals("1 - list contains 1 element", 1, contextNodes.size());
    assertEquals("2 - ", firstr, expectedFirstRoot);
    
    //Now we are ready to do an reaction our match will be 'r'
    //But first the thing we want to react to
    Node a = NodeFactory.instance().createElementNode("A");
    Node b = NodeFactory.instance().createElementNode("B");
    List<Node> firstrChilds = new ArrayList<Node>(2);
    firstrChilds.add(a);
    firstrChilds.add(b);
    Node secondR = NodeFactory.instance().createElementNode("r", firstrChilds);
    drs.react(con, firstr, secondR);
    
    //Now we try to get a new EvaluationContext
    xpath = new XPath("//*", new XMLStoreNavigator());
    con = drs.getEvaluationContext(xpath);
    
    //If the above reaction went well this context should now contain A and B
    contextNodes = con.getContextNodes();
    assertEquals("3 - list contains 2 el.", 2, contextNodes.size());
    Node weWantA = (Node) contextNodes.get(0);
    Node weWantB = (Node) contextNodes.get(1);
    assertEquals("4 - ", a, weWantA);
    assertEquals("5 - ", b, weWantB);
    
    //So far so good now we want to do some parallel reactions
    //First we get a new Context and changes A
    xpath = new XPath("//child::*", new XMLStoreNavigator());
    EvaluationContext con1 = drs.getEvaluationContext(xpath);
    Node c = NodeFactory.instance().createElementNode("C");
    Node d = NodeFactory.instance().createElementNode("D");
    List<Node> achilds = new ArrayList<Node>(2);
    achilds.add(c);
    achilds.add(d);
    Node a1 = NodeFactory.instance().createElementNode("A", achilds);
    drs.react(con1, a, a1);
    //Now we use the old context in a reaction where we also changes a
    //this should raise an exception
    Node e = NodeFactory.instance().createElementNode("E");
    try {
      drs.react(con, a, e);
      //If we get this far its a failure
      fail("6 - Didn't threw a ConflictException");
    } catch(ConflictException ce) {
      //We are happy and continues testing :-)
    }
    //lets make an reaction on b with this old context
    //this should be fine
    drs.react(con, b, e);
    
    //Ok - time to get a new context and see if graph looks like
    //supposed
    xpath = new XPath("self::*", new XMLStoreNavigator());
    con = drs.getEvaluationContext(xpath);
    //The graph sould now consist af an r node with children a1 and e
    contextNodes = con.getContextNodes();
    Node actualR = (Node) contextNodes.get(0);
    Node expectedR = NodeFactory.instance().replaceChild(
        NodeFactory.instance().replaceChild(secondR, 0, a1), 1,e);
    assertEquals("7 - ", expectedR,actualR);
    
    //More tests to come....
  }
  
  protected void tearDown() {
    File f1 = new File("testStore.data");
    File f2 = new File("testStore.free");
    File f3 = new File("testStore.localns.map");
    f1.delete();
    f2.delete();
    f3.delete();
  }
  
  public static void main(String args[]) {
    TestRunner.run(DistributedReactiveStore.class);
  }
}
