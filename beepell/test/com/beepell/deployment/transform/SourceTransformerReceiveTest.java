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
public class SourceTransformerReceiveTest extends TestCase {

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
        File source = new File("test/com/beepell/deployment/transform/sef-receive-test-case.bpel");
        DocumentBuilder builder = factory.newDocumentBuilder();
        document = builder.parse(source);
        document.normalizeDocument();

        transformed = SourceTransformer.transform(document);

        //write(transformed, new File("trash/sef-receive-test-case-result.xml"));
        
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
     * Testing 
     */
    public final void testCoreReceive() {
        
        assertEquals(1, getNodes("/bpel:process/bpel:sequence/bpel:receive[@name='core']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core' and @suppressJoinFailure = 'yes']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core' and @partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core' and @operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core' and @variable = 'request']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core' and @messageExchange = 'mea']").getLength());
        
        assertEquals(1, getNodes("//bpel:receive[@name='core']/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core']/bpel:targets/bpel:target").getLength());

        assertEquals(1, getNodes("//bpel:receive[@name='core']/bpel:sources/bpel:source").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core']/bpel:sources/bpel:source/bpel:transitionCondition").getLength());

    }    

    /**
     * Testing 
     */
    public final void testNovariableReceive() {
        
        assertEquals(1, getNodes("/bpel:process/bpel:sequence/bpel:receive[@name='novariable']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='novariable' and @suppressJoinFailure = 'yes']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='novariable' and @partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='novariable' and @operation = 'noticeService']").getLength());
        assertEquals(0, getNodes("//bpel:receive[@name='novariable' and @variable]").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='novariable' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='novariable' and @messageExchange = 'meb']").getLength());
    }    

    /**
     * Testing 
     */
    public final void testFromPartsReceive() {
        
        // Check the invoke activity
        assertEquals(1, getNodes("/bpel:process/bpel:sequence/bpel:scope[@name='fromparts']/bpel:sequence/bpel:receive[@name='fromparts']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='fromparts' and @partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='fromparts' and @operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='fromparts' and @variable = 'temporaryOutputMessage']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='fromparts' and not(bpel:fromParts)]").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='fromparts' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='fromparts' and @messageExchange = 'mec']").getLength());
        
        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:variables/bpel:variable[@name = 'temporaryOutputMessage' and @messageType='srv:requestMessage']").getLength());
        assertEquals(2, getNodes("//bpel:scope[@name='fromparts']/bpel:sequence/*").getLength());
        
        // Check the assignments
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:sequence/bpel:assign").getLength());
        assertEquals(2, getNodes("//bpel:scope[@name='fromparts']/bpel:sequence/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:sequence/bpel:assign/bpel:copy/bpel:from[@variable='temporaryOutputMessage' and @part='id']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:sequence/bpel:assign/bpel:copy/bpel:to[@variable='idVariable']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:sequence/bpel:assign/bpel:copy/bpel:from[@variable='temporaryOutputMessage' and @part='description']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:sequence/bpel:assign/bpel:copy/bpel:to[@variable='descriptionVariable' ]").getLength());
                
    }
    
    /**
     * 
     *
     */
    public final void testElementReceive() {
        
        assertEquals(1, getNodes("/bpel:process/bpel:sequence/bpel:scope[@name='elements']/bpel:sequence/bpel:receive[@name='elements']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='elements' and @partnerLink   = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='elements' and @operation     = 'simpleService']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='elements' and @variable = 'temporaryOutputMessage']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='elements' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='elements' and @messageExchange = 'med']").getLength());
        
        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:variables/bpel:variable[@name = 'temporaryOutputMessage' and @messageType='srv:simpleRequestMessage']").getLength());
        assertEquals(2, getNodes("//bpel:scope[@name='elements']/bpel:sequence/*").getLength());
        
        // Check the assignments
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:sequence/bpel:assign").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:sequence/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:sequence/bpel:assign/bpel:copy/bpel:from[@variable='temporaryOutputMessage' and @part='addressIn']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:sequence/bpel:assign/bpel:copy/bpel:to[@variable='addressVariable']").getLength());
    }

}
