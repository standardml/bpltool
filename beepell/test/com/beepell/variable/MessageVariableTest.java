package com.beepell.variable;

import java.io.File;
import java.net.URI;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import com.beepell.exceptions.UninitializedVariable;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;


/**
 * @author Tim Hallwyl
 *
 */
public class MessageVariableTest extends TestCase {

    private QName qName = new QName("http://beepell.com/samples/dummy/service", "aMessage");

    /**
     * Test constructor.
     */
    public final void testConstructor() {
        try {
            SchemaRepository schemas = new SchemaRepository();
            URI wsdl = new File("./test/com/beepell/variable/service.wsdl").toURI();
            schemas.addWSDL(wsdl);
            
            ServiceRepository services = new ServiceRepository();
            services.add(wsdl);           
            
            MessageVariable variable = new MessageVariable(qName, "variable", schemas, services);

            assertEquals(qName, variable.getType());
            assertEquals("variable", variable.getName());
            assertFalse(variable.isInitialized());

        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }
    }
    
    /**
     * 
     *
     */
    public final void testPartiallyInitialize() {
        try {
            SchemaRepository schemas = new SchemaRepository();
            URI wsdl = new File("./test/com/beepell/variable/service.wsdl").toURI();
            schemas.addWSDL(wsdl);
            
            ServiceRepository services = new ServiceRepository();
            services.add(wsdl);           
            
            MessageVariable variable = new MessageVariable(qName, "variable", schemas, services);
            assertFalse(variable.isInitialized());

            try {
                variable.getValue("manager");
                fail("Expected an exception.");
            } catch (UninitializedVariable exception) {
                assertTrue(true);
            }

            variable.initialize("manager");
            
            Node value = null;
            try {
                value = variable.getValue("manager");
            } catch (UninitializedVariable exception) {
                exception.printStackTrace();
                fail("Caught unexpected exception");
            }

            assertNotNull(value);
            assertTrue(value instanceof Element);
            assertEquals(value.getOwnerDocument().getDocumentElement(), value);
            assertEquals(0, value.getChildNodes().getLength());

            assertEquals("person", value.getLocalName());
            assertEquals("http://beepell.com/samples/dummy/schema", value.getNamespaceURI());
            
            variable.uninitialize("manager");
            
            try {
                variable.getValue("manager");
                fail("Expected an exception.");
            } catch (UninitializedVariable exception) {
                assertTrue(true);
            }
            
            variable.initialize("manager");
            
            try {
                value = variable.getValue("manager");
            } catch (UninitializedVariable exception) {
                exception.printStackTrace();
                fail("Caught unexpected exception");
            }
            
        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }      
        
    }
    /**
     * Test variable initalization.
     */
    public final void testInitialize() {
        try {
            SchemaRepository schemas = new SchemaRepository();
            URI wsdl = new File("./test/com/beepell/variable/service.wsdl").toURI();
            schemas.addWSDL(wsdl);
            
            ServiceRepository services = new ServiceRepository();
            services.add(wsdl);           
            
            MessageVariable variable = new MessageVariable(qName, "variable", schemas, services);
            assertFalse(variable.isInitialized());

            try {
                variable.getValue("manager");
                fail("Expected an exception.");
            } catch (UninitializedVariable exception) {
                assertTrue(true);
            }

            variable.initialize();
            assertTrue(variable.isInitialized());

            Node value = null;

            // ------------------------------------------------------------------
            // Part test: manager (person element)
            
            try {
                value = variable.getValue("manager");
            } catch (UninitializedVariable exception) {
                fail("Caught unexpected exception");
            }

            assertNotNull(value);
            assertTrue(value instanceof Element);
            assertEquals(value.getOwnerDocument().getDocumentElement(), value);
            assertEquals(0, value.getChildNodes().getLength());

            assertEquals("person", value.getLocalName());
            assertEquals("http://beepell.com/samples/dummy/schema", value.getNamespaceURI());

            // ------------------------------------------------------------------
            // Part test: name (personinfo complex type)
            
            try {
                value = variable.getValue("name");
            } catch (UninitializedVariable exception) {
                fail("Caught unexpected exception");
            }

            assertNotNull(value);
            assertTrue(value instanceof Element);
            assertEquals(value.getOwnerDocument().getDocumentElement(), value);
            assertEquals(0, value.getChildNodes().getLength());

            // Namespace uri and local name of the element is not specified by the 
            // standard, but an implementation must specify some value.
            assertNotNull(value.getLocalName());
            assertNotNull(value.getNamespaceURI());

            // ------------------------------------------------------------------
            // Part test: isUseful (simple type)
            
            try {
                value = variable.getValue("isUseful");
            } catch (UninitializedVariable exception) {
                fail("Caught unexpected exception");
            }

            assertNotNull(value);
            assertTrue(value instanceof Text);
            assertEquals(value.getOwnerDocument().getDocumentElement().getFirstChild(), value);
            assertEquals(0, value.getChildNodes().getLength());

            // Namespace uri and local name of the element is not specified by the 
            // standard, but an implementation must specify some value.
            assertNotNull(value.getParentNode().getLocalName());
            assertNotNull(value.getParentNode().getNamespaceURI());

            

        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }
    }
    
}
