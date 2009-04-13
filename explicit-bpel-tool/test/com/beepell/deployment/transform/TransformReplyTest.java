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
        
        assertEquals(1, getNodes("//bpel:reply[@name='core']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='core' and @suppressJoinFailure = 'yes']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='core' and @partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='core' and @operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='core' and @variable = 'response']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='core' and @messageExchange = 'mesex']").getLength());
        
        assertEquals(1, getNodes("//bpel:reply[@name='core']/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='core']/bpel:targets/bpel:target[@linkName='ship-to-invoice']").getLength());

        assertEquals(1, getNodes("//bpel:reply[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']/bpel:transitionCondition").getLength());

    }    

    /**
     * Testing 
     */
    public final void testNovariableReply() {
        
        assertEquals(1, getNodes("//bpel:reply[@name='novariable']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='novariable' and @suppressJoinFailure = 'yes']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='novariable' and @partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='novariable' and @operation = 'noticeService']").getLength());
        assertEquals(0, getNodes("//bpel:reply[@name='novariable' and @variable]").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='novariable' and @messageExchange = 'mesex']").getLength());
    }    

    /**
     * Testing 
     */
    public final void testToPartsReply() {
        
        // Check the invoke activity
        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:flow/bpel:reply[@name='toparts']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='toparts' and @partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='toparts' and @operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='toparts' and @variable = 'v0InputMessage']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='toparts' and not(bpel:toParts)]").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='toparts' and @messageExchange = 'mesex']").getLength());
        
        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:variables/bpel:variable[@name = 'v0InputMessage' and @messageType='srv:responseMessage']").getLength());
        assertEquals(3, getNodes("//bpel:scope[@name='toparts']/bpel:flow/*").getLength());
        
        // Check the assignments
        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:flow/bpel:assign").getLength());
        assertEquals(2, getNodes("//bpel:scope[@name='toparts']/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='v0InputMessage' and @part='id']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='idVariable']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='v0InputMessage' and @part='description']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='descriptionVariable' ]").getLength());
                
    }
    
    /**
     * 
     *
     */
    public final void testElementReceive() {
        
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:reply[@name='elements']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='elements' and @partnerLink   = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='elements' and @operation     = 'simpleService']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='elements' and @variable = 'v0InputMessage']").getLength());
        assertEquals(1, getNodes("//bpel:reply[@name='elements' and @messageExchange = 'mesex']").getLength());
        
        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:variables/bpel:variable[@name = 'v0InputMessage' and @messageType='srv:simpleResponseMessage']").getLength());
        assertEquals(3, getNodes("//bpel:scope[@name='elements']/bpel:flow/*").getLength());
        
        // Check the assignments
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='v0InputMessage' and @part='addressOut']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='addressVariable']").getLength());
    }

}
