package com.beepell.deployment;

import java.io.File;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import junit.framework.TestCase;

import org.w3c.dom.Document;

import com.beepell.deployment.transform.SourceTransformer;
import com.beepell.model.InvokeActivity;
import com.beepell.model.ProcessDescription;
import com.beepell.model.SequenceActivity;
import com.beepell.util.XML;

/**
 * Test of the JAXB UnMarshaller
 * @author Tim Hallwyl
 *
 */
public class TestJAXBUnMarshaller extends TestCase {

    private static final SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);

    /**
     * Testing unmarshalling -- particular of expressions into element nodes.
     *
     */
    @SuppressWarnings("unchecked")
	public final void testUnMarshaller() {

        try {
            File file = new File(TestJAXBUnMarshaller.class.getResource("transform/sef-invoke-test-case.bpel").toURI());
            Schema schema = schemaFactory.newSchema(new File("schemas/bpel.xsd"));

            Document sef = SourceTransformer.transform(XML.read(file, schema));

            JAXBContext jc = JAXBContext.newInstance("com.beepell.model");
            Unmarshaller unmarshaller = jc.createUnmarshaller();
            JAXBElement element = (JAXBElement) unmarshaller.unmarshal(sef);
            ProcessDescription process = (ProcessDescription) element.getValue();

            assertNotNull(process);

            InvokeActivity invoke = (InvokeActivity) ((SequenceActivity) process.getActivity()).getActivity().get(1);
            assertNotNull(invoke);
            assertNotNull(invoke.getTargets());
//          TODO: assertNotNull(invoke.getTargets().getJoinCondition());
//          TODO: assertNotNull(invoke.getTargets().getJoinCondition().getTextContent());
//          TODO: assertEquals("fn:true()", invoke.getTargets().getJoinCondition().getTextContent().trim());

//          TODO: Element node = invoke.getSources().getSource().get(0).getTransitionCondition();
//          TODO: assertEquals("urn:one", node.lookupNamespaceURI("one"));
//          TODO: assertEquals("urn:two", node.lookupNamespaceURI("two"));
//          TODO: assertEquals("urn:three", node.lookupNamespaceURI("three"));
            
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception.");
        }

    }

}
