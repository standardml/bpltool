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

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import com.beepell.deployment.transform.SourceTransformer;
import com.beepell.xml.namespace.DocumentNamespaceContext;
import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;

/**
 * @author Tim Hallwyl
 * 
 */
public class TransformerScopeTest extends TestCase {

    private Document transformed;
    private Document document;
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
        File source = new File("test/com/beepell/deployment/transform/sef-scope-test-case.bpel");
        DocumentBuilder builder = factory.newDocumentBuilder();
        document = builder.parse(source);
        document.normalizeDocument();

        transformed = SourceTransformer.transform(document);

        //write(transformed, new File("trash/sef-scope-test-case-result.xml"));
        
        NamespaceContext namespaceContext = new DocumentNamespaceContext(transformed);
        xPath.setNamespaceContext(namespaceContext);

    }

    @SuppressWarnings("unused")
	private void write(Document document, File file) throws IOException {
        file.createNewFile();
        OutputFormat format = new OutputFormat(document);
        format.setIndenting(true);
        XMLSerializer serializer = new XMLSerializer(new FileOutputStream(file), format);
        serializer.serialize(document);
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
        boolean isOriginalDocument = true;

        try {
            Schema schema = schemaFactory.newSchema(new File("schemas/bpel.xsd"));
            Validator validator = schema.newValidator();
            validator.validate(new DOMSource(document));
            isOriginalDocument = false;
            validator.validate(new DOMSource(transformed));
            
        } catch (SAXParseException e) {
            String message;
            System.err.println("Line" + e.getLineNumber() + " at " + e.getColumnNumber() + ":" + e.getLocalizedMessage());
            if (isOriginalDocument)
                message = "Original document is not valid BPEL: " + e.getLocalizedMessage();
            else
                message = "Transformed document is not valid BPEL: " + e.getLocalizedMessage();
                
            fail(message);
        } catch (SAXException e) {
            e.printStackTrace();
            fail("Document is not a valid BPEL document: " + e.getLocalizedMessage());
        } catch (Exception e) {
            e.printStackTrace();
            fail("Document is not a valid BPEL document: " + e.getLocalizedMessage());
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
     * 
     *
     */
    public final void testDefaultHandlers() {
               
        assertEquals(1, getNodes("/bpel:process/bpel:sequence/bpel:scope[@name='simple']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='simple']/bpel:faultHandlers").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='simple']/bpel:compensationHandler").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='simple']/bpel:terminationHandler").getLength());

        assertEquals(1, getNodes("//bpel:scope[@name='core']/bpel:faultHandlers").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='core']/bpel:compensationHandler").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='core']/bpel:terminationHandler").getLength());
        
        assertEquals(1, getNodes("//bpel:scope[@name='empty']/bpel:faultHandlers").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='empty']/bpel:compensationHandler").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='empty']/bpel:terminationHandler").getLength());
       
        assertEquals(1, getNodes("//bpel:scope[@name='termination']/bpel:faultHandlers").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='termination']/bpel:compensationHandler").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='termination']/bpel:terminationHandler").getLength());
            
    }    
    
 
    
}
