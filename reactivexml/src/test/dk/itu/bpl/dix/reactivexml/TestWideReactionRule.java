package dk.itu.bpl.dix.reactivexml;
import dk.itu.bpl.dix.store.*;

import java.io.*;
import java.util.*;
import org.planx.xmlstore.references.LocalLocator;
import org.planx.xmlstore.stores.*;
import org.planx.xmlstore.input.*;
import org.planx.xmlstore.*;
import org.planx.xpath.XMLStoreNavigator;
import org.planx.xpath.XPath;

import junit.framework.TestCase;
import junit.textui.TestRunner;

public class TestWideReactionRule extends TestCase {
  
  private String storeName = "testWide";
  
  
  public void testWide() throws Exception {
    //Get rid of XML Store files on shutdown.
    /*
    Runtime.getRuntime().addShutdownHook(new Thread(){
      public void run(){
        File f1 = new File(storeName + ".data");
        File f2 = new File(storeName + ".free");
        File f3 = new File(storeName + ".localns.map");
        if(f1.exists()) f1.delete();
        if(f2.exists()) f2.delete();
        if(f3.exists()) f3.delete();
        
      }
    });
    
    XMLStore<LocalLocator> xstore = new LocalXMLStore(storeName);
    NameServer<LocalLocator> ns = xstore.getNameServer();
    DistributedReactiveStore<LocalLocator> store = new DistributedReactiveStore<LocalLocator>(xstore, ns);
    store.init("src/test/WideRuleTestProcess.xml");
    Node wideRule = SAXBuilder.build("src/test/WideRulesTest.xml");
    WideRewriteRule wrr = new WideRewriteRule(wideRule);
    EvaluationContext context = store.getEvaluationContext(new XPath("descendant-or-self::*", 
        new XMLStoreNavigator()));
    List<CompleteWideMatch> matches = wrr.findReactions(context);
    List<? extends Node> contextNodes = context.getContextNodes();
    //
    //System.out.println("No of matches: " + matches.size());
    //for(CompleteWideMatch match : matches) {
    //  System.out.println(match);
    //}
    
    //System.out.println("LAST MATCH");
    CompletePrimeMatch last = matches.get(0).getMatches().get(2);
     
    //System.out.println("Hole map: " + last.getHoleMap());
    //System.out.println("Location:\n" + last.getLocation());
    //System.out.println("Rigth: \n" + last.getRight());
    //System.out.println("Reactum: \n" + last.createReactum());
    
    */
  }
  public static void main(String args[]) {
    TestRunner.run(TestWideReactionRule.class);
  }
}
