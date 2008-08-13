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
public class SourceTransformerInvokeTest extends TestCase {

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
        File source = new File("test/com/beepell/deployment/transform/sef-invoke-test-case.bpel");
        DocumentBuilder builder = factory.newDocumentBuilder();
        document = builder.parse(source);
        document.normalizeDocument();

        transformed = SourceTransformer.transform(document);

        //write(transformed, new File("trash/sef-invoke-test-case-result.xml"));
        
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

    /*
    private int getInteger(String path) {
        try {
            return ((Double) (xPath.evaluate(path, transformed, XPathConstants.NUMBER))).intValue();
        } catch (XPathExpressionException e) {
            e.printStackTrace();
        }
        return 0;
    }
    */

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
    public final void testCoreInvoke() {
        
        assertEquals(1, getNodes("/bpel:process/bpel:sequence/bpel:invoke[@name='core']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='core' and @partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='core' and @operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='core' and @inputVariable = 'request']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='core' and @outputVariable = 'response']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='core' and @suppressJoinFailure = 'yes']").getLength());
        
        assertEquals(1, getNodes("//bpel:invoke[@name='core']/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='core']/bpel:targets/bpel:target").getLength());

        assertEquals(1, getNodes("//bpel:invoke[@name='core']/bpel:sources/bpel:source").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='core']/bpel:sources/bpel:source/bpel:transitionCondition").getLength());

        assertEquals(1, getNodes("//bpel:invoke[@name='core']/bpel:correlations").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='core']/bpel:correlations/bpel:correlation[@set='PurchaseOrder' and @pattern='request']").getLength());
        
    }    
    
    
    
    /**
     * 
     *
     */
    public final void testLocalScope() {
        assertEquals(1, getNodes("/bpel:process/bpel:sequence/bpel:scope[@name='localscope']/bpel:invoke[@name='localscope']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='localscope' and @partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='localscope' and @operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='localscope' and @inputVariable = 'request']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='localscope' and @outputVariable = 'response']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='localscope']/bpel:correlations/bpel:correlation[@set='PurchaseOrder' and @pattern='request']").getLength());
        
        assertEquals(1, getNodes("//bpel:scope[@name='localscope' and @suppressJoinFailure='yes']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:targets/bpel:target").getLength());

        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:sources/bpel:source").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:sources/bpel:source/bpel:transitionCondition").getLength());

        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:faultHandlers/bpel:catch[@faultName='bpel:correlationViolation']/bpel:empty[@name='catch']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:faultHandlers/bpel:catchAll/bpel:empty[@name='catchAll']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:compensationHandler/bpel:empty[@name='compensationHandler']").getLength());

        assertEquals(1, getNodes("//bpel:invoke[@name='localscope']/bpel:correlations").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='localscope']/bpel:correlations/bpel:correlation[@set='PurchaseOrder' and @pattern='request']").getLength());

    }
    
    /**
     * 
     *
     */
    public final void testToFromPartsInvoke() {
        
        // Check the invoke activity
        assertEquals(1, getNodes("/bpel:process/bpel:sequence/bpel:scope[@name='tofromparts']/bpel:sequence/bpel:invoke[@name='tofromparts']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='tofromparts' and @partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='tofromparts' and @operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='tofromparts' and @inputVariable = 'temporaryInputMessage']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='tofromparts' and @outputVariable = 'temporaryOutputMessage']").getLength());

        assertEquals(1, getNodes("//bpel:invoke[@name='tofromparts' and not(bpel:toParts)]").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='tofromparts' and not(bpel:fromParts)]").getLength());

        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:variables/bpel:variable[@name = 'temporaryInputMessage' and @messageType='srv:requestMessage']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:variables/bpel:variable[@name = 'temporaryOutputMessage' and @messageType='srv:responseMessage']").getLength());
        assertEquals(3, getNodes("//bpel:scope[@name='tofromparts']/bpel:sequence/*").getLength());
        
        // Check the assignments
        assertEquals(2, getNodes("//bpel:scope[@name='tofromparts']/bpel:sequence/bpel:assign").getLength());
        assertEquals(4, getNodes("//bpel:scope[@name='tofromparts']/bpel:sequence/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:sequence/bpel:assign[1]/bpel:copy/bpel:from[@variable='idVariable']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:sequence/bpel:assign[1]/bpel:copy/bpel:to[@variable='temporaryInputMessage' and @part='id']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:sequence/bpel:assign[1]/bpel:copy/bpel:from[@variable='descriptionVariable']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:sequence/bpel:assign[1]/bpel:copy/bpel:to[@variable='temporaryInputMessage' and @part='description']").getLength());
        
        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:sequence/bpel:assign[2]/bpel:copy/bpel:from[@variable='temporaryOutputMessage' and @part='approved']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:sequence/bpel:assign[2]/bpel:copy/bpel:to[@variable='approvedVariable']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:sequence/bpel:assign[2]/bpel:copy/bpel:from[@variable='temporaryOutputMessage' and @part='note']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:sequence/bpel:assign[2]/bpel:copy/bpel:to[@variable='noteVariable' ]").getLength());
        
        // Secondary tests: toParts alone
        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:sequence/bpel:assign").getLength());
        assertEquals(2, getNodes("//bpel:scope[@name='toparts']/bpel:sequence/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:sequence/bpel:assign/bpel:copy/bpel:from[@variable='idVariable']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:sequence/bpel:assign/bpel:copy/bpel:to[@variable='temporaryInputMessage' and @part='id']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:sequence/bpel:assign/bpel:copy/bpel:from[@variable='descriptionVariable']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:sequence/bpel:assign/bpel:copy/bpel:to[@variable='temporaryInputMessage' and @part='description']").getLength());

        // Secondary tests: fromParts alone
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:sequence/bpel:assign").getLength());
        assertEquals(2, getNodes("//bpel:scope[@name='fromparts']/bpel:sequence/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:sequence/bpel:assign/bpel:copy/bpel:from[@variable='temporaryOutputMessage' and @part='approved']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:sequence/bpel:assign/bpel:copy/bpel:to[@variable='approvedVariable']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:sequence/bpel:assign/bpel:copy/bpel:from[@variable='temporaryOutputMessage' and @part='note']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:sequence/bpel:assign/bpel:copy/bpel:to[@variable='noteVariable' ]").getLength());

                
    }
    
    /**
     * 
     *
     */
    public final void testElementsInvoke() {
        
        assertEquals(1, getNodes("/bpel:process/bpel:sequence/bpel:scope[@name='elements']/bpel:sequence/bpel:invoke[@name='elements']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='elements' and @partnerLink   = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='elements' and @operation     = 'simpleService']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='elements' and @inputVariable = 'temporaryInputMessage']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='elements' and @outputVariable = 'temporaryOutputMessage']").getLength());
        
        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:variables/bpel:variable[@name = 'temporaryInputMessage' and @messageType='srv:simpleRequestMessage']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:variables/bpel:variable[@name = 'temporaryOutputMessage' and @messageType='srv:simpleResponseMessage']").getLength());
        assertEquals(3, getNodes("//bpel:scope[@name='elements']/bpel:sequence/*").getLength());
        
        // Check the assignments
        assertEquals(2, getNodes("//bpel:scope[@name='elements']/bpel:sequence/bpel:assign").getLength());
        assertEquals(2, getNodes("//bpel:scope[@name='elements']/bpel:sequence/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:sequence/bpel:assign[1]/bpel:copy/bpel:from[@variable='addressVariable']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:sequence/bpel:assign[1]/bpel:copy/bpel:to[@variable='temporaryInputMessage' and @part='addressIn']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:sequence/bpel:assign[2]/bpel:copy/bpel:from[@variable='temporaryOutputMessage' and @part='addressOut']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:sequence/bpel:assign[2]/bpel:copy/bpel:to[@variable='addressVariable']").getLength());
    }
    /**
     * 
     *
     */
    public final void testMixedInvoke() {
        
        assertEquals(1, getNodes("/bpel:process/bpel:sequence/bpel:scope[@name='mixed']/bpel:sequence/bpel:invoke[@name='mixed']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='mixed' and @partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='mixed' and @operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='mixed' and @inputVariable = 'temporaryInputMessage']").getLength());
        assertEquals(1, getNodes("//bpel:invoke[@name='mixed' and @outputVariable = 'temporaryOutputMessage']").getLength());
                
        // Testing scope
        assertEquals(1, getNodes("//bpel:scope[@name='mixed']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='mixed' and @suppressJoinFailure='yes']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:targets/bpel:target").getLength());

        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:sources/bpel:source").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:sources/bpel:source/bpel:transitionCondition").getLength());

        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:faultHandlers/bpel:catch[@faultName='bpel:correlationViolation']/bpel:empty[@name='catch']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:faultHandlers/bpel:catchAll/bpel:empty[@name='catchAll']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:compensationHandler/bpel:empty[@name='compensationHandler']").getLength());

        assertEquals(2, getNodes("//bpel:scope[@name='mixed']/bpel:sequence/bpel:assign").getLength());
        assertEquals(3, getNodes("//bpel:scope[@name='mixed']/bpel:sequence/bpel:assign/bpel:copy").getLength());
        
        // Testing toParts
        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:sequence/bpel:assign/bpel:copy/bpel:from[@variable='idVariable']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:sequence/bpel:assign/bpel:copy/bpel:to[@variable='temporaryInputMessage' and @part='id']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:sequence/bpel:assign/bpel:copy/bpel:from[@variable='descriptionVariable']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:sequence/bpel:assign/bpel:copy/bpel:to[@variable='temporaryInputMessage' and @part='description']").getLength());

        // Testing element part
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:sequence/bpel:assign[2]/bpel:copy/bpel:from[@variable='temporaryOutputMessage' and @part='addressOut']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:sequence/bpel:assign[2]/bpel:copy/bpel:to[@variable='addressVariable']").getLength());
     
        
    }
    
    
}
