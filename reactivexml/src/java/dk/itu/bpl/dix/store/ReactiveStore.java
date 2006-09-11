package dk.itu.bpl.dix.store;

import org.planx.xmlstore.*;
import org.planx.xpath.XPath;
import org.planx.xpath.XPathException;
import java.io.IOException;
import java.util.*;

public interface ReactiveStore {

  // TODO Declare new exceptions

  public EvaluationContext getEvaluationContext(XPath xpath)
      throws IOException, NameServerException;

  public void react(EvaluationContext context, Node match, Node reactum)
      throws IOException, ConflictException, XPathException,
      NameServerException;
  
  public void reactWide(EvaluationContext context, Map<Node, Node> wideReactions)
    throws IOException, ConflictException, XPathException,
      NameServerException;

  public Node getProcessTree() throws StoreNotInitializedException,
      IOException;

  public void init(String file) throws IOException, NameAlreadyBoundException,
      NameServerException;

}
