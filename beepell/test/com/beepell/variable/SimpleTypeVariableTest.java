package com.beepell.variable;

import java.io.File;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.w3c.dom.Node;
import org.w3c.dom.Text;

import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.repository.SchemaRepository;

/**
 * Tests SimpleTypeVariable
 * @author Tim Hallwyl
 *
 */
public class SimpleTypeVariableTest extends TestCase {
    private QName qName = new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "boolean");
    private SchemaRepository repository;

    /**
     * Test constructor.
     */
    public final void testConstructor() {
        try {
            repository = new SchemaRepository();
            repository.add(new File("./test/com/beepell/variable/schema.xsd").toURI());
            
            SimpleTypeVariable variable = new SimpleTypeVariable(qName, "variable", repository);

            assertEquals(qName, variable.getType());
            assertEquals("variable", variable.getName());
            assertFalse(variable.isInitialized());

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
            repository = new SchemaRepository();
            repository.add(new File("./test/com/beepell/variable/schema.xsd").toURI());
            SimpleTypeVariable variable = new SimpleTypeVariable(qName, "variable", repository);
            assertFalse(variable.isInitialized());

            try {
                variable.getValue();
                fail("Expected an exception.");
            } catch (UninitializedVariable exception) {
                assertTrue(true);
            }

            variable.initialize();
            assertTrue(variable.isInitialized());

            Node value = null;

            try {
                value = variable.getValue();
            } catch (UninitializedVariable exception) {
                fail("Caught unexpected exception");
            }

            assertNotNull(value);
            assertTrue(value instanceof Text);
            assertEquals(value.getOwnerDocument().getDocumentElement().getFirstChild(), value);
            assertEquals(0, value.getChildNodes().getLength());

            // Namespace uri and local name of the enclosing element is not specified by the 
            // standard, but an implementation must specify some value.
            assertNotNull(value.getParentNode().getLocalName());
            assertNotNull(value.getParentNode().getNamespaceURI());

        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }
    }
    
    /**
     * Test variable validation.
     */
    public final void testValidation() {
        try {
            repository = new SchemaRepository();
            repository.add(new File("./test/com/beepell/variable/schema.xsd").toURI());
            SimpleTypeVariable variable = new SimpleTypeVariable(qName, "variable", repository);

            try {
                variable.validate();
                fail("Expected an exception.");
            } catch (UninitializedVariable exception) {
                assertTrue(true);
            }

            variable.initialize();

            try {
                variable.validate();
                fail("Expected an exception.");
            } catch (InvalidVariables exception) {
                assertTrue(true);
            }

            Text value = variable.getValue();
            value.setData("false");
            
            try {
                variable.validate();                
            } catch (InvalidVariables exception) {
                exception.printStackTrace();
                fail("Caught unexpected exception");
            }
            

        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }
    }
    
    
}
