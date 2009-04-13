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
 * 
 */
public class TransformReceiveTest extends TestCase {

    private Document transformed;
    private XPath xPath = XPathFactory.newInstance().newXPath();

    @Override
    protected void setUp() throws Exception {
        super.setUp();

        File source = new File("test/com/beepell/deployment/transform/receive.bpel");
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
    public final void testCoreReceive() {
        
        assertEquals(1, getNodes("//bpel:receive[@name='core']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core' and @suppressJoinFailure = 'yes']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core' and @partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core' and @operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core' and @variable = 'request']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core' and @messageExchange = 'mea']").getLength());
        
        assertEquals(1, getNodes("//bpel:receive[@name='core']/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core']/bpel:targets/bpel:target").getLength());

        assertEquals(1, getNodes("//bpel:receive[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']/bpel:transitionCondition").getLength());

    }    

    /**
     * Testing 
     */
    public final void testNovariableReceive() {
        
        assertEquals(1, getNodes("//bpel:receive[@name='novariable']").getLength());
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
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:receive[@name='fromparts']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='fromparts' and @partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='fromparts' and @operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='fromparts' and @variable = 'v0OutputMessage']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='fromparts' and not(bpel:fromParts)]").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='fromparts' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='fromparts' and @messageExchange = 'mec']").getLength());
        
        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:variables/bpel:variable[@name = 'v0OutputMessage' and @messageType='srv:requestMessage']").getLength());
        assertEquals(3, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/*").getLength());
        
        // Check the assignments
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:assign").getLength());
        assertEquals(2, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='v0OutputMessage' and @part='id']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='idVariable']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='v0OutputMessage' and @part='description']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='descriptionVariable' ]").getLength());
                
    }
    
    /**
     * 
     *
     */
    public final void testElementReceive() {
        
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:receive[@name='elements']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='elements' and @partnerLink   = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='elements' and @operation     = 'simpleService']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='elements' and @variable = 'v0OutputMessage']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='elements' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:receive[@name='elements' and @messageExchange = 'med']").getLength());
        
        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:variables/bpel:variable[@name = 'v0OutputMessage' and @messageType='srv:simpleRequestMessage']").getLength());
        assertEquals(3, getNodes("//bpel:scope[@name='elements']/bpel:flow/*").getLength());
        
        // Check the assignments
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='v0OutputMessage' and @part='addressIn']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='addressVariable']").getLength());
    }

}
