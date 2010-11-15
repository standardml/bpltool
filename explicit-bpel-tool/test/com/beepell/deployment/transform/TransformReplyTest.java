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
 * 
 */
public class TransformReplyTest extends TestCase {

    private Document transformed;
    private XPath xPath = XPathFactory.newInstance().newXPath();

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        File source = new File("test/com/beepell/deployment/transform/reply.bpel");
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
    public final void testCoreReply() {
        
        assertEquals(1, getNodes("//bpel:flow[@name='core']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core' and @suppressJoinFailure = 'yes']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:reply[@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:reply[@operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:reply[@variable = 'response']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:reply[@messageExchange = 'mesex']").getLength());
        
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:targets/bpel:target[@linkName='ship-to-invoice']").getLength());

        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']/bpel:transitionCondition").getLength());

    }    

    /**
     * Testing 
     */
    public final void testNovariableReply() {
        
        assertEquals(1, getNodes("//bpel:flow[@name='novariable']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='novariable' and @suppressJoinFailure = 'yes']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='novariable']/bpel:reply[@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='novariable']/bpel:reply[@operation = 'noticeService']").getLength());
        assertEquals(0, getNodes("//bpel:flow[@name='novariable']/bpel:reply[@variable]").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='novariable']/bpel:reply[@messageExchange = 'mesex']").getLength());
    }    

    /**
     * Testing 
     */
    public final void testToPartsReply() {
        
        // Check the reply activity
        assertEquals(1, getNodes("//bpel:flow[@name='toparts']/bpel:reply").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='toparts']/bpel:reply[@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='toparts']/bpel:reply[@operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='toparts']/bpel:reply[not(bpel:toParts)]").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='toparts']/bpel:reply[@messageExchange = 'mesex']").getLength());
        
        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:flow[@name='toparts']/ancestor::bpel:scope[1]/bpel:variables/bpel:variable[@messageType='srv:responseMessage']").getLength());
        assertEquals(3, getNodes("//bpel:flow[@name='toparts']/ancestor::bpel:scope[1]/bpel:flow/*").getLength());
        
        // Check the assignments
        assertEquals(1, getNodes("//bpel:flow[@name='toparts']/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign").getLength());
        assertEquals(2, getNodes("//bpel:flow[@name='toparts']/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='toparts']/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:to[@part='id']").getLength());
        assertEquals(2, getNodes("//bpel:flow[@name='toparts']/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:from").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='toparts']/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:to[@part='description']").getLength());
                
    }
    
    /**
     * 
     *
     */
    public final void testElementReceive() {
        
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/bpel:reply").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/bpel:reply[@partnerLink     = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/bpel:reply[@operation       = 'simpleService']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/bpel:reply[@messageExchange = 'mesex']").getLength());
        
        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/ancestor::bpel:scope[1]/bpel:variables/bpel:variable[@messageType='srv:simpleResponseMessage']").getLength());
        assertEquals(3, getNodes("//bpel:flow[@name='elements']/ancestor::bpel:scope[1]/bpel:flow/*").getLength());
        
        // Check the assignments
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:to[@part='addressOut']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:from").getLength());
    }

}
