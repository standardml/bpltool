package com.beepell.variable;

import java.io.File;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.repository.SchemaRepository;

/**
 * @author Tim Hallwyl
 */
public class ElementVariableTest extends TestCase {

    private QName qName = new QName("http://beepell.com/samples/dummy/schema", "person");
    private SchemaRepository repository;

    /**
     * Test constructor.
     */
    public final void testConstructor() {
        try {
            repository = new SchemaRepository();
            repository.add(new File("./test/com/beepell/variable/schema.xsd").toURI());
            ElementVariable variable = new ElementVariable(qName, "variable", repository);

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
            ElementVariable variable = new ElementVariable(qName, "variable", repository);
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

            assertEquals(qName.getLocalPart(), value.getLocalName());
            assertEquals(qName.getNamespaceURI(), value.getNamespaceURI());

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
            ElementVariable variable = new ElementVariable(qName, "variable", repository);

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

            Node value = variable.getValue();
            Document document = value.getOwnerDocument();
            Element person = getPerson();
            assertNotNull(person);
            person = (Element) document.importNode(person, true);  
            assertNotNull(person);
            document.replaceChild(person, value);
            
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

    private Element getPerson() {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setValidating(false);
            factory.setNamespaceAware(true);
            DocumentBuilder builder;
            builder = factory.newDocumentBuilder();
            Document document = builder.parse("test/com/beepell/variable/document.xml");
            assertNotNull(document);
            Element element = document.getDocumentElement();
            assertNotNull(element);
            Element person = (Element) element.getElementsByTagNameNS(qName.getNamespaceURI(), qName.getLocalPart()).item(0);
            assertNotNull(person);
                        
            return person;

        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
            return null;
        }
    }

}
