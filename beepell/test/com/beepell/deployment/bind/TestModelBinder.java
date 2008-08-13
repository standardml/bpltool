package com.beepell.deployment.bind;

import java.io.File;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.w3c.dom.Document;

import com.beepell.deployment.transform.SourceTransformer;
import com.beepell.model.*;

import junit.framework.TestCase;

/**
 * 
 * @author Tim Hallwyl
 *
 */
public class TestModelBinder extends TestCase {

    private Document sbpel = null;
    
    protected void setUp() throws Exception {
        
        SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        Schema bpelSchema = schemaFactory.newSchema(new File("schemas/bpel.xsd"));
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setSchema(bpelSchema);
        factory.setValidating(false);
        factory.setCoalescing(false);
        factory.setIgnoringComments(true);
        factory.setIgnoringElementContentWhitespace(true);
        factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, false);
        factory.setNamespaceAware(true);

        File source = new File("test/com/beepell/deployment/bind/sef-extensions-test-case.bpel");
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document bpel = builder.parse(source);
        
        sbpel = SourceTransformer.transform(bpel);
        bpelSchema.newValidator().validate(new DOMSource(sbpel));
        Schema sbpelSchema = schemaFactory.newSchema(new File("schemas/s-bpel.xsd"));
        sbpelSchema.newValidator().validate(new DOMSource(sbpel));

        
    }
    
    /**
     * 
     *
     */
    public final void testBinding() {
        
        assertNotNull(sbpel);
        
        try {
          ProcessDescription process = ModelBinder.bind(sbpel);
          assertNotNull(process);

          SequenceActivity sequence = (SequenceActivity) process.getActivity();
          assertNotNull(sequence);

          ReceiveActivity receive = (ReceiveActivity) sequence.getActivity().get(0);
          assertNotNull(receive);
          
          assertEquals("ping", receive.getPartnerLink());
          assertEquals("pingOperation", receive.getOperation());
          assertEquals("in", receive.getVariable());
          assertEquals(true, receive.isCreateInstance());
          
        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception");
        }
        
    }
    
    /*
    public final void testExitOnStandardFault() {
        
        assertNotNull(sbpel);
        
        try {
            
          ProcessDescription process = ModelBinder.bind(sbpel);
          assertNotNull(process);
          
          assertEquals(false, process.isExitOnStandardFault());
         
          SequenceActivity sequence = (SequenceActivity) process.getActivity();
          assertNotNull(sequence);
          
          ScopeActivity scope = (ScopeActivity) sequence.getActivity().get(5);
          assertEquals(new Boolean(false), scope.isExitOnStandardFault());
          
          
        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception");
        } 
    }
    */
    
}
