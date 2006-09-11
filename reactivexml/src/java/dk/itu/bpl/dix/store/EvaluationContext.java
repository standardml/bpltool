package dk.itu.bpl.dix.store;

import java.util.*;

import org.apache.log4j.Logger;
import org.planx.xmlstore.*;
import org.planx.xpath.*;
import org.planx.xpath.object.XNodeSet;

import dk.itu.bpl.dix.reactivexml.MatchFunctions;

/**
 * @author Martin Olsen
 * @version $Revision: 1.6 $
 */

public class EvaluationContext {
  private static final Logger logger 
  = Logger.getLogger(EvaluationContext.class);
  private Node root;
    private XPath xpath;
    private List<? extends Node> contextNodes = null;
    
    
    protected EvaluationContext(XPath xpath, Node root) {
        this.root = root;
        this.xpath = xpath;
    }
/**
 * This method will perform the XPath specifying the EvaluationContext<br> 
 * If the XPath evaluation fails <code>null</code> is returned
 * @return A list of context nodes
 */
    public List<? extends Node> getContextNodes() {
      try {  
      //Lazy evauluation af Xpath'en
        if(contextNodes == null) {
            XPath rpath = new XPath("/Root/Version/wide", new XMLStoreNavigator());
            DocNode graph = (DocNode) ((XNodeSet) rpath.evaluate(new DocNode(root))).get(0);
            logger.debug("Graph: " + graph);
            contextNodes = (XNodeSet) xpath.evaluate(new DocNode(graph));
        }
        return contextNodes;
      } catch (XPathException xe) {
        return null;
      }
    }
    
    protected Node getRoot() {
        return root;
    }
}
