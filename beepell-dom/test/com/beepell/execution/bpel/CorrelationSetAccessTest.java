package com.beepell.execution.bpel;

import static org.junit.Assert.*;

import java.io.File;
import java.net.URI;

import javax.xml.namespace.QName;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * @author Tim Hallwyl
 *
 */
public class CorrelationSetAccessTest {

    private Element instance;
    
    private XPath xPath;
    
    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        
        SchemaRepository schemas = new SchemaRepository();
        URI xsd = VariableAccessTest.class.getResource("correlationSetAccessTest.xsd").toURI();
        schemas.add(xsd);
        
        ServiceRepository services = new ServiceRepository();
        
        File file = new File(VariableAccessTest.class.getResource("correlationSetAccessTest.bpi").toURI());    
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
     * Test method for {@link com.beepell.execution.bpel.Context#getCorrelationSet(java.lang.String)}.
     */
    @Test
    public final void testGetCorrelationSet() {
        try {
            Node node, value;
            Context context;
            
            node = evaluate("//bpi:empty[@name='A']", this.instance);
            context = new Context(node);
            
            value = context.getCorrelationSet("order");
            assertNotNull(value);
            assertNull(value.getFirstChild());

            value = context.getCorrelationSet("invoice");
            assertNotNull(value);
            assertEquals("42", evaluate("bpi:property[@name='ws:pa']/text()", value).getNodeValue().trim());
            assertEquals("Headfield", evaluate("bpi:property[@name='ws:pb']/bpi:value/hr:lastname/text()", value).getNodeValue().trim());
            assertEquals("Ulrich", evaluate("bpi:property[@name='ws:pc']/hr:person/hr:lastname/text()", value).getNodeValue().trim());
            
            node = evaluate("//bpi:empty[@name='B']", this.instance);
            context = new Context(node);
            
            value = context.getCorrelationSet("order");
            assertNotNull(value);
            assertNull(value.getFirstChild());

            value = context.getCorrelationSet("invoice");
            assertNotNull(value);
            assertEquals("12", evaluate("bpi:property[@name='ws:pa']/text()", value).getNodeValue().trim());
            assertEquals("Headfield", evaluate("bpi:property[@name='ws:pb']/bpi:value/hr:lastname/text()", value).getNodeValue().trim());
            assertEquals("Hammett", evaluate("bpi:property[@name='ws:pc']/hr:person/hr:lastname/text()", value).getNodeValue().trim());            
            
            
        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }

    /**
     * Test method for {@link com.beepell.execution.bpel.Context#getCorrelationValue(java.lang.String, javax.xml.namespace.QName)}.
     */
    @Test
    public final void testGetCorrelationValue() {
        try {
            final String ws = "http://beepell.com/samples/dummy/service"; 
            Node node, value;
            Context context;
            
            node = evaluate("//bpi:empty[@name='A']", this.instance);
            context = new Context(node);
            
            value = context.getCorrelationValue("invoice", new QName(ws, "pa"));
            assertNotNull(value);
            assertTrue(value instanceof Text);
            assertEquals("42", value.getNodeValue().trim());
            
            value = context.getCorrelationValue("invoice", new QName(ws, "pb"));
            assertNotNull(value);
            assertTrue(value instanceof Element);
            assertEquals("value", value.getLocalName());
            assertEquals("Headfield", evaluate("hr:lastname/text()", value).getNodeValue().trim());
            
            value = context.getCorrelationValue("invoice", new QName(ws, "pc"));
            assertNotNull(value);
            assertEquals("person", value.getLocalName());
            assertTrue(value instanceof Element);
            assertEquals("Ulrich", evaluate("hr:lastname/text()", value).getNodeValue().trim());
            
        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }

}
