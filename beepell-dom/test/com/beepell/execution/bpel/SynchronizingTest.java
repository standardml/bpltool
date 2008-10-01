package com.beepell.execution.bpel;

import static org.junit.Assert.*;

import java.io.File;

import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * @author Tim Hallwyl
 *
 */
public class SynchronizingTest {

    private Element instance;
    private XPath xPath;
    
    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        
        SchemaRepository schemas = new SchemaRepository();
        
        ServiceRepository services = new ServiceRepository();
        
        File file = new File(VariableAccessTest.class.getResource("synchronizingTest.bpi").toURI());    
        this.instance = Utils.load(file);
        Document document = this.instance.getOwnerDocument();
        document.setUserData("com.beepell.repository.SchemaRepository", schemas, null);
        document.setUserData("com.beepell.repository.ServiceRepository", services, null);
    
        XPathFactory factory = XPathFactory.newInstance();
        this.xPath = factory.newXPath();
        this.xPath.setNamespaceContext(new DocumentNamespaceContext(this.instance.getOwnerDocument()));
    }
    
    private Node evaluate(final String expression, final Node contextNode) throws XPathExpressionException {
        return (Node) this.xPath.evaluate(expression, contextNode, XPathConstants.NODE);
    }
    /**
     * Test method for {@link com.beepell.execution.bpel.Context#isSynchronizing()}.
     */
    @Test
    public final void testIsSynchronizing() {
        try {
            Node node;
            Context context;
            boolean value;
            
            node = evaluate("//bpi:*[@name='b2']", this.instance);
            assertNotNull(node);
            context = new Context(node);
            value = context.isSynchronizing();
            assertTrue(value);
            
            node = evaluate("//bpi:*[@name='B']", this.instance);
            assertNotNull(node);
            context = new Context(node);
            value = context.isSynchronizing();
            assertFalse(value);

            node = evaluate("//bpi:*[@name='b1']", this.instance);
            assertNotNull(node);
            context = new Context(node);
            value = context.isSynchronizing();
            assertFalse(value);

            node = evaluate("//bpi:instance/bpi:flow", this.instance);
            assertNotNull(node);
            context = new Context(node);
            value = context.isSynchronizing();
            assertFalse(value);
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }

}
