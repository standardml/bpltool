/**
 * 
 */
package com.beepell.deployment.transform;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import junit.framework.TestCase;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.beepell.xml.namespace.DocumentNamespaceContext;
import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;

/**
 * @author Tim Hallwyl
 * 
 */
public class SourceTransformerExtensionsTest extends TestCase {

    private Document transformed;

    private XPath xPath = XPathFactory.newInstance().newXPath();

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        
        SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        Schema schema = schemaFactory.newSchema(new File("schemas/bpel.xsd"));
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setSchema(schema);
        factory.setValidating(false);
        factory.setCoalescing(false);
        factory.setIgnoringComments(true);
        factory.setIgnoringElementContentWhitespace(true);
        factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, false);
        factory.setNamespaceAware(true);
        File source = new File("test/com/beepell/deployment/transform/sef-extensions-test-case.bpel");
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.parse(source);
        document.normalizeDocument();
        transformed = SourceTransformer.transform(document);
        //write(transformed, new File("trash/sef-extensions-test-case-result.xml"));
        
        NamespaceContext namespaceContext = new DocumentNamespaceContext(document);
        xPath.setNamespaceContext(namespaceContext);

    }

    private NodeList getNodes(String path) {
        try {
            return (NodeList) xPath.evaluate(path, transformed, XPathConstants.NODESET);
        } catch (XPathExpressionException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Testing transformation produced a valid BPEL and BPEL SEF document.
     * {@link com.beepell.deployment.transform.SourceTransformer#transform(org.w3c.dom.Document)}.
     */
    public final void testTransform() {
        assertNotNull(transformed);
        SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);

        try {
            Schema schema = schemaFactory.newSchema(new File("schemas/bpel.xsd"));
            Validator validator = schema.newValidator();
            validator.validate(new DOMSource(transformed));
            
        } catch (SAXException e) {
            System.out.println(e.toString());
            fail("Document is not a valid BPEL document: " + e.getLocalizedMessage());
            
            e.printStackTrace();
        } catch (Exception e) {
            fail("Document is not a valid BPEL document: " + e.getLocalizedMessage());
            e.printStackTrace();
        }

        
        try {
            Schema schema = schemaFactory.newSchema(new File("schemas/s-bpel.xsd"));
            Validator validator = schema.newValidator();
            validator.validate(new DOMSource(transformed));
            
        } catch (Exception e) {
            fail("Document is not a valid BPEL SEF document: " + e.getLocalizedMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Testing removal of expressionLanguage and queryLanguage attributes.
     *
     */
    public final void testLanguageAttributes() {
        // No extension attributes should still be there
        assertEquals(0, getNodes("//@expressionLanguage").getLength());
        
        // No extension attributes should still be there
        assertEquals(0, getNodes("//@queryLanguage").getLength());
    }

    /**
     * Testing removal of extension attributes and elements. All elements and
     * attributes from other namespaces than BPEL should be removed.
     * 
     */
    public final void testAttributeAndElementExtensions() {

        // No extension attributes should still be there
        assertEquals(0, getNodes("//@ext:*").getLength());
        // No extension elements should still be there
        assertEquals(0, getNodes("//ext:*").getLength());

        // No elements from outside BPEL, besides 2 elements in literal assign, should still be there
        assertEquals(2, getNodes("//*[namespace-uri() != 'http://docs.oasis-open.org/wsbpel/2.0/process/executable']").getLength());

        // No attributes from outside BPEL should still be there
        assertEquals(0, getNodes("//@*[namespace-uri() != '']").getLength());
    }

    /**
     * Testing removal of extensionAssignOperation from assign elements. Three
     * cases: - the assign contains no extensionAssignOperation and is
     * untouched. - the assign contains both extensionAssignOperation and copy,
     * extensionAssignOperation is removed. - the assign contains only
     * extensionAssignOperation and is replaced by an empty activity.
     */
    public final void testExtensionAssignOperation() {

        // Out of three assign, two should still be there
        assertEquals(3, getNodes("//bpel:assign").getLength());
        assertEquals(1, getNodes("//bpel:assign[count(child::bpel:copy) = 2]").getLength());
        assertEquals(2, getNodes("//bpel:assign[count(child::bpel:copy) = 1]").getLength());

        // All extensionAssignOperation should be gone
        assertEquals(0, getNodes("//bpel:extensionAssignOperation").getLength());

        // In total there should be three copy (in the two assign elements)
        assertEquals(4, getNodes("//bpel:copy").getLength());

        // Test transformation of assign to empty
        assertEquals(1, getNodes("//bpel:empty[@name='AssignWithoutCopy']").getLength());
        assertEquals(1, getNodes("//bpel:empty[@name='AssignWithoutCopy']/bpel:targets/bpel:target[@linkName='someother']").getLength());
        assertEquals(1, getNodes("//bpel:empty[@name='AssignWithoutCopy']/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("//bpel:empty[@name='AssignWithoutCopy']/bpel:sources/bpel:source[@linkName='someone']").getLength());
        assertEquals(1, getNodes("//bpel:empty[@name='AssignWithoutCopy']/bpel:sources/bpel:source[@linkName=\'someone\']/bpel:transitionCondition").getLength());

    }

    /**
     * Testing removal of documentation elements.
     */
    public final void testDocumentation() {
        assertEquals(0, getNodes("//bpel:documentation").getLength());
    }

    /**
     * Testing replacing of extension activities with empty activities.
     * 
     */
    public final void testExtensionActivities() {
        // No extension activities should still be there
        assertEquals(0, getNodes("//bpel:extensionActivity").getLength());
        assertEquals(0, getNodes("//ext:myActivity").getLength());

        // Extension activituy holding MyActivity should be replaced with an
        // empty
        assertEquals(1, getNodes("//bpel:empty[@name='MyExtensionActivity']").getLength());
        assertEquals(1, getNodes("//bpel:empty[@name='MyExtensionActivity' and @suppressJoinFailure='yes']").getLength());
        assertEquals(1, getNodes("//bpel:empty[@name='MyExtensionActivity']/bpel:targets/bpel:target[@linkName='someone']").getLength());
        assertEquals(1, getNodes("//bpel:empty[@name='MyExtensionActivity']/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("//bpel:empty[@name='MyExtensionActivity']/bpel:sources/bpel:source[@linkName='someother']").getLength());
        assertEquals(1, getNodes("//bpel:empty[@name='MyExtensionActivity']/bpel:sources/bpel:source[@linkName='someother']/bpel:transitionCondition").getLength());
    }
    
    /**
     * Test to show that we keep namespace declarations.
     *
     */
    public final void testNamespaceDeclarations() {
        Element receive = getElement("//bpel:receive");
        assertNotNull(receive);

        assertEquals("urn:one", receive.lookupNamespaceURI("one"));
        assertEquals("urn:two", receive.lookupNamespaceURI("two"));
        
        Element reply = getElement("//bpel:reply");
        assertNotNull(reply);

        assertEquals("urn:one", reply.lookupNamespaceURI("one"));
        assertEquals("urn:two", reply.lookupNamespaceURI("two"));
        
        Element assign = getElement("//bpel:*[@name='AssignWithoutCopy']");
        assertNotNull(assign);

        assertEquals("urn:foo", assign.lookupNamespaceURI("one"));
        assertEquals("urn:bar", assign.lookupNamespaceURI("two"));

        Element condition = getElement("//bpel:*[@name='AssignWithoutCopy']/bpel:targets/bpel:joinCondition");
        assertNotNull(condition);

        assertEquals("urn:one", condition.lookupNamespaceURI("one"));
        assertEquals("urn:two", condition.lookupNamespaceURI("two")); 
    }
    
    @SuppressWarnings("unused")
    private Attr getAttr(String xpath) {
        return (Attr) getNodes(xpath).item(0);
    }
    
    private Element getElement(String xpath) {
        return (Element) getNodes(xpath).item(0);
    }
    
    @SuppressWarnings("unused")
    private void write(Document document, File file) throws IOException {
        file.createNewFile();
        OutputFormat format = new OutputFormat(document);
        format.setIndenting(true);
        XMLSerializer serializer = new XMLSerializer(new FileOutputStream(file), format);
        serializer.serialize(document);
    }
    
    /**
     * Bugfix: literal values are destroyed: turned into a getTextContent like value.
     *
     */
    public void testBug001() {
        Element literal = (Element) getNodes("//bpel:assign[@name='literal']/bpel:copy/bpel:from/bpel:literal").item(0);
        assertEquals("Peter",  literal.getElementsByTagNameNS("http://tim.hallwyl.dk/insurance", "firstname").item(0).getTextContent());
        assertEquals("Ulrich", literal.getElementsByTagNameNS("http://tim.hallwyl.dk/insurance", "lastname").item(0).getTextContent());
        //System.out.print(XML.toString(literal));
        
    }
}
