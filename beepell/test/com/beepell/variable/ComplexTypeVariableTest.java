package com.beepell.variable;

import java.io.File;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.repository.SchemaRepository;


/**
 * @author Tim Hallwyl
 *
 */
public class ComplexTypeVariableTest extends TestCase {

    
    private QName qName = new QName("http://beepell.com/samples/dummy/schema", "personinfo");
    private SchemaRepository repository;

    /**
     * Test constructor.
     */
    public final void testConstructor() {
        try {
            repository = new SchemaRepository();


            
            ComplexTypeVariable variable = new ComplexTypeVariable(qName, "variable", repository);

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
            ComplexTypeVariable variable = new ComplexTypeVariable(qName, "variable", repository);
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
            assertTrue(value instanceof Element);
            assertEquals(value.getOwnerDocument().getDocumentElement(), value);
            assertEquals(0, value.getChildNodes().getLength());

            // Namespace uri and local name of the element is not specified by the 
            // standard, but an implementation must specify some value.
            assertNotNull(value.getLocalName());
            assertNotNull(value.getNamespaceURI());

            
            
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
            ComplexTypeVariable variable = new ComplexTypeVariable(qName, "variable", repository);

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

            
            Element value = variable.getValue();
            Document document = value.getOwnerDocument();
            
            Element firstname = document.createElementNS(qName.getNamespaceURI(), "firstname");
            firstname.setNodeValue("Simon");
            value.appendChild(firstname);

            Element lastname = document.createElementNS(qName.getNamespaceURI(), "lastname");
            firstname.setNodeValue("Spies");
            value.appendChild(lastname);

            
            try {
                variable.validate();                
            } catch (InvalidVariables exception) {
                exception.printStackTrace();
                fail("Caught unexpected exception");
            }
            
            testInitialize();
            
        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }
    }
    
    
}
