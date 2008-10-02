package com.beepell.execution.bpel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.net.URI;

import javax.xml.xpath.XPathExpressionException;

import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;

/**
 * @author Tim Hallwyl
 *
 */
public class VariableAccessTest extends AbstractContextTest{

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        
        SchemaRepository schemas = new SchemaRepository();
        URI wsdl = VariableAccessTest.class.getResource("variableAccessTest.wsdl").toURI();
        schemas.addWSDL(wsdl);
        
        ServiceRepository services = new ServiceRepository();
        services.add(wsdl);
        
        load(new File(VariableAccessTest.class.getResource("variableAccessTest.bpi").toURI()));    
        Document document = this.getInstance().getOwnerDocument();
        document.setUserData("com.beepell.repository.SchemaRepository", schemas, null);
        document.setUserData("com.beepell.repository.ServiceRepository", services, null);
    }

    /**
     * Test method for {@link com.beepell.execution.bpel.Context#getVariableValue(java.lang.String)}.
     */
    @Test
    public final void testGetUnitializedVariable() {
        try {
            Node node, value;
            Context context;
            
            node = evaluate("//bpi:instance/bpi:sequence", this.getInstance());
            context = new Context(node);
            
            value = context.getVariableValue("foo");
            assertNull(value);
            
        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }

    }
    
    /**
     * Test method for {@link com.beepell.execution.bpel.Context#getVariableValue(java.lang.String)}.
     */
    @Test
    public final void testGetSimpleTypeVariableValue() {
        try {
            Node node, value;
            Context context;
            
            node = evaluate("//bpi:scope[@name='A']/bpi:empty", this.getInstance());
            context = new Context(node);
            
            value = context.getVariableValue("bar");
            assertTrue(value instanceof Text);
            assertEquals("42", value.getNodeValue().trim());
            
            value = context.getVariableValue("foo");
            assertTrue(value instanceof Text);
            assertEquals("A", value.getNodeValue().trim());

            
            node = evaluate("//bpi:scope[@name='B']/bpi:empty", this.getInstance());
            context = new Context(node);
            
            value = context.getVariableValue("bar");
            assertTrue(value instanceof Text);
            assertEquals("42", value.getNodeValue().trim());
            
            value = context.getVariableValue("foo");
            assertTrue(value instanceof Text);
            assertEquals("B", value.getNodeValue().trim());


            node = evaluate("//bpi:scope[@name='C']/bpi:scope", this.getInstance());
            context = new Context(node);
            
            value = context.getVariableValue("bar");
            assertTrue(value instanceof Text);
            assertEquals("42", value.getNodeValue().trim());
            
            value = context.getVariableValue("foo");
            assertTrue(value instanceof Text);
            assertEquals("C", value.getNodeValue().trim());

            
            node = evaluate("//bpi:scope[@name='D']/bpi:empty", this.getInstance());
            context = new Context(node);
            
            value = context.getVariableValue("bar");
            assertTrue(value instanceof Text);
            assertEquals("42", value.getNodeValue().trim());
            
            value = context.getVariableValue("foo");
            assertTrue(value instanceof Text);
            assertEquals("D", value.getNodeValue().trim());

            value = context.getVariableValue("kung");
            assertTrue(value instanceof Text);
            assertEquals("12", value.getNodeValue().trim());

            
        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }
    
    /**
     * Test method for {@link com.beepell.execution.bpel.Context#getVariableValue(java.lang.String)}.
     */
    @Test
    public final void testGetComplexTypeVariableValue() {
        try {
            Node node, value;
            Context context;
            
            node = evaluate("//bpi:scope[@name='A']/bpi:empty", this.getInstance());
            context = new Context(node);
            
            value = context.getVariableValue("kung");
            assertTrue(value instanceof Element);
            assertEquals("value", value.getLocalName());

            // Relative to the context node:
            assertEquals("James", evaluate("hr:firstname/text()", value).getNodeValue());
            assertEquals("Headfield", evaluate("hr:lastname/text()", value).getNodeValue());
                        
        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }

    }

    /**
     * Test method for {@link com.beepell.execution.bpel.Context#getVariableValue(java.lang.String)}.
     */
    @Test
    public final void testGetElementTypeVariableValue() {
        try {
            Node node, value;
            Context context;
            
            node = evaluate("//bpi:scope[@name='B']/bpi:empty", this.getInstance());
            context = new Context(node);
            
            value = context.getVariableValue("kung");
            assertTrue(value instanceof Element);

            // Relative to the context node:
            assertEquals("Lars", evaluate("hr:firstname/text()", value).getNodeValue());
            assertEquals("Ulrich", evaluate("hr:lastname/text()", value).getNodeValue());
            assertEquals("45", evaluate("hr:age/text()", value).getNodeValue());
            assertEquals("1963-12-26T09:30:10Z", evaluate("hr:dateborn/text()", value).getNodeValue());
            assertEquals("P5Y2M10D", evaluate("hr:lease/text()", value).getNodeValue());
            
            
        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }
    
    /**
     * Test method for {@link com.beepell.execution.bpel.Context#getVariableValue(java.lang.String, java.lang.String)}.
     */
    @Test
    public final void testGetMessageVariablePartValue() {
        try {
            Node node, value;
            Context context;
            
            node = evaluate("//bpi:scope[@name='D']", this.getInstance());
            context = new Context(node);
            
            value = context.getVariableValue("kung", "manager");
            assertTrue(value instanceof Element);

            // Relative to the context node:
            assertEquals("Lars", evaluate("hr:firstname/text()", value).getNodeValue());
            assertEquals("Ulrich", evaluate("hr:lastname/text()", value).getNodeValue());
            assertEquals("45", evaluate("hr:age/text()", value).getNodeValue());
            assertEquals("1963-12-26T09:30:10Z", evaluate("hr:dateborn/text()", value).getNodeValue());
            assertEquals("P5Y2M10D", evaluate("hr:lease/text()", value).getNodeValue());
            
            value = context.getVariableValue("kung", "name");
            assertTrue(value instanceof Element);

            // Relative to the context node:
            assertEquals("James", evaluate("hr:firstname/text()", value).getNodeValue());
            assertEquals("Headfield", evaluate("hr:lastname/text()", value).getNodeValue());
            
            value = context.getVariableValue("kung", "isUseful");
            assertTrue(value instanceof Text);
            assertEquals("false", value.getNodeValue().trim());

            
            
        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }

}
