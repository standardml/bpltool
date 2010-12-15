package com.beepell.deployment.transform;

import java.io.File;

import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * @author Tim Hallwyl
 * @author Espen H¿jsgaard
 */
public class TransformOnEventTest extends TestCase {

    private Document transformed;
    private XPath xPath = XPathFactory.newInstance().newXPath();

    @Override
    protected void setUp() throws Exception {
        super.setUp();

        File source = new File("test/com/beepell/deployment/transform/onEvent.bpel");
        Transform transform = new Transform();
        transformed = transform.transform(source);

        NamespaceContext namespaceContext = new DocumentNamespaceContext(transformed);
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
     * Testing
     */
    public final void testCoreOnEvent() {

        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[1]").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[1][@partnerLink = 'eventPartnerLink']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[1][@operation   = 'requestService']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[1][@messageType = 'srv:requestMessage']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[1][@variable    = 'request']").getLength());

        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[1]/bpel:scope").getLength());
        
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[1]/bpel:scope/bpel:partnerLinks").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[1]/bpel:scope/bpel:partnerLinks/bpel:partnerLink").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[1]/bpel:scope/bpel:partnerLinks/bpel:partnerLink[@name = 'eventPartnerLink']").getLength());

        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[1]/bpel:scope/bpel:flow[@name = 'emptyCore']/bpel:empty").getLength());
    }

    /**
     * Testing
     */
    public final void testFromPartsOnEvent() {

        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2]").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2][not(bpel:fromParts)]").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2][@partnerLink     = 'eventPartnerLink']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2][@operation       = 'requestService']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2][@messageType     = 'srv:requestMessage']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2][@variable]").getLength());

        // Check the scope
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2]/bpel:scope/bpel:variables/bpel:variable[@name='idVariable']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2]/bpel:scope/bpel:variables/bpel:variable[@type='xsd:integer']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2]/bpel:scope/bpel:variables/bpel:variable[@name='descriptionVariable']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2]/bpel:scope/bpel:variables/bpel:variable[@type='xsd:string']").getLength());

        // Check the assignments
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2]/bpel:scope/bpel:flow/bpel:flow[1]/bpel:assign").getLength());
        assertEquals(2, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2]/bpel:scope/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2]/bpel:scope/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy[1]/bpel:from[@part='id']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2]/bpel:scope/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy[1]/bpel:to[@variable='idVariable']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2]/bpel:scope/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy[2]/bpel:from[@part='description']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2]/bpel:scope/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy[2]/bpel:to[@variable='descriptionVariable']").getLength());

        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[2]/bpel:scope/bpel:flow/bpel:flow[2]/bpel:flow[@name = 'emptyFromParts']/bpel:empty").getLength());
    }

    /**
     * 
     *
     */
    public final void testElementOnEvent() {

        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[3]").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[3][not(bpel:fromParts)]").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[3][@partnerLink     = 'eventPartnerLink']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[3][@operation       = 'simpleService']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[3][@messageType     = 'srv:simpleRequestMessage']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[3][@variable]").getLength());

        // Check the scope
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[3]/bpel:scope/bpel:variables/bpel:variable[@name='addressVariable']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[3]/bpel:scope/bpel:variables/bpel:variable[@element='srv:addressType']").getLength());

        // Check the assignments
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[3]/bpel:scope/bpel:flow/bpel:flow[1]/bpel:assign").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[3]/bpel:scope/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[3]/bpel:scope/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy[1]/bpel:from[@part='addressIn']").getLength());
        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[3]/bpel:scope/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy[1]/bpel:to[@variable='addressVariable']").getLength());

        assertEquals(1, getNodes("/bpel:process//bpel:scope[1]/bpel:eventHandlers/bpel:onEvent[3]/bpel:scope/bpel:flow/bpel:flow[2]/bpel:flow[@name = 'emptyElements']/bpel:empty").getLength());
}

}
