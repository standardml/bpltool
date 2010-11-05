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
public class TransformInvokeTest extends TestCase {

    private Transform transform;
    private Document transformed;
    private XPath xPath = XPathFactory.newInstance().newXPath();

    @Override
    protected void setUp() throws Exception {
        super.setUp();

        File source = new File("test/com/beepell/deployment/transform/invoke.bpel");
        this.transform = new Transform();
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
     * 
     *
     */
    public final void testCoreInvoke() {
        //FIXME name attributes are removed from all activities but <scope>s
//        assertEquals(1, getNodes("//bpel:invoke[@name='core']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='core' and @partnerLink = 'serverPartnerLink']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='core' and @operation = 'requestService']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='core' and @inputVariable = 'request']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='core' and @outputVariable = 'response']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='core' and @suppressJoinFailure = 'yes']").getLength());
//        
//        assertEquals(1, getNodes("//bpel:invoke[@name='core']/bpel:targets/bpel:joinCondition").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='core']/bpel:targets/bpel:target[@linkName='ship-to-invoice']").getLength());
//
//        assertEquals(1, getNodes("//bpel:invoke[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']/bpel:transitionCondition").getLength());
//
//        assertEquals(1, getNodes("//bpel:invoke[@name='core']/bpel:correlations").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='core']/bpel:correlations/bpel:correlation[@set='PurchaseOrder' and @pattern='request']").getLength());
        
    }    
    
    
    
    /**
     * 
     *
     */
    public final void testLocalScope() {
        //FIXME name attributes are removed from all activities but <scope>s
//        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:invoke[@name='localscope']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='localscope' and @partnerLink = 'serverPartnerLink']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='localscope' and @operation = 'requestService']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='localscope' and @inputVariable = 'request']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='localscope' and @outputVariable = 'response']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='localscope']/bpel:correlations/bpel:correlation[@set='PurchaseOrder' and @pattern='request']").getLength());
//        
//        assertEquals(1, getNodes("//bpel:scope[@name='localscope' and @suppressJoinFailure='yes']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:targets/bpel:joinCondition").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:targets/bpel:target[@linkName='ship-to-invoice']").getLength());
//
//        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']/bpel:transitionCondition").getLength());
//
//        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:faultHandlers/bpel:catch[@faultName='bpel:correlationViolation']/bpel:empty[@name='catch']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:faultHandlers/bpel:catchAll/bpel:empty[@name='catchAll']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='localscope']/bpel:compensationHandler/bpel:empty[@name='compensationHandler']").getLength());
//
//        assertEquals(1, getNodes("//bpel:invoke[@name='localscope']/bpel:correlations").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='localscope']/bpel:correlations/bpel:correlation[@set='PurchaseOrder' and @pattern='request']").getLength());

    }
    
    /**
     * 
     *
     */
    public final void testToFromPartsInvoke() {
        
        //FIXME name attributes are removed from all activities but <scope>s
//        // Check the invoke activity
//        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:flow/bpel:invoke[@name='tofromparts']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='tofromparts' and @partnerLink = 'serverPartnerLink']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='tofromparts' and @operation = 'requestService']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='tofromparts' and @inputVariable = 'v0InputMessage']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='tofromparts' and @outputVariable = 'v0OutputMessage']").getLength());
//
//        assertEquals(1, getNodes("//bpel:invoke[@name='tofromparts' and not(bpel:toParts)]").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='tofromparts' and not(bpel:fromParts)]").getLength());
//
//        // Check the enclosing scope
//        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:variables/bpel:variable[@name = 'v0InputMessage' and @messageType='srv:requestMessage']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:variables/bpel:variable[@name = 'v0OutputMessage' and @messageType='srv:responseMessage']").getLength());
//        assertEquals(4, getNodes("//bpel:scope[@name='tofromparts']/bpel:flow/*").getLength());
//        
//        // Check the assignments
//        assertEquals(2, getNodes("//bpel:scope[@name='tofromparts']/bpel:flow/bpel:assign").getLength());
//        assertEquals(4, getNodes("//bpel:scope[@name='tofromparts']/bpel:flow/bpel:assign/bpel:copy").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:flow/bpel:assign[1]/bpel:copy/bpel:from[@variable='idVariable']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:flow/bpel:assign[1]/bpel:copy/bpel:to[@variable='v0InputMessage' and @part='id']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:flow/bpel:assign[1]/bpel:copy/bpel:from[@variable='descriptionVariable']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:flow/bpel:assign[1]/bpel:copy/bpel:to[@variable='v0InputMessage' and @part='description']").getLength());
//        
//        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:flow/bpel:assign[2]/bpel:copy/bpel:from[@variable='v0OutputMessage' and @part='approved']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:flow/bpel:assign[2]/bpel:copy/bpel:to[@variable='approvedVariable']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:flow/bpel:assign[2]/bpel:copy/bpel:from[@variable='v0OutputMessage' and @part='note']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='tofromparts']/bpel:flow/bpel:assign[2]/bpel:copy/bpel:to[@variable='noteVariable' ]").getLength());
//        
//        // Secondary tests: toParts alone
//        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:flow/bpel:assign").getLength());
//        assertEquals(2, getNodes("//bpel:scope[@name='toparts']/bpel:flow/bpel:assign/bpel:copy").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='idVariable']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='v0InputMessage' and @part='id']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='descriptionVariable']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='toparts']/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='v0InputMessage' and @part='description']").getLength());
//
//        // Secondary tests: fromParts alone
//        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:assign").getLength());
//        assertEquals(2, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:assign/bpel:copy").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='v0OutputMessage' and @part='approved']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='approvedVariable']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='v0OutputMessage' and @part='note']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='noteVariable' ]").getLength());

                
    }
    
    /**
     * 
     *
     */
    public final void testElementsInvoke() {
        
        //FIXME name attributes are removed from all activities but <scope>s
//        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:invoke[@name='elements']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='elements' and @partnerLink   = 'serverPartnerLink']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='elements' and @operation     = 'simpleService']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='elements' and @inputVariable = 'v0InputMessage']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='elements' and @outputVariable = 'v0OutputMessage']").getLength());
//        
//        // Check the enclosing scope
//        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:variables/bpel:variable[@name = 'v0InputMessage' and @messageType='srv:simpleRequestMessage']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:variables/bpel:variable[@name = 'v0OutputMessage' and @messageType='srv:simpleResponseMessage']").getLength());
//        assertEquals(4, getNodes("//bpel:scope[@name='elements']/bpel:flow/*").getLength());
//        
//        // Check the assignments
//        assertEquals(2, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign").getLength());
//        assertEquals(2, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign/bpel:copy").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign[1]/bpel:copy/bpel:from[@variable='addressVariable']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign[1]/bpel:copy/bpel:to[@variable='v0InputMessage' and @part='addressIn']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign[2]/bpel:copy/bpel:from[@variable='v0OutputMessage' and @part='addressOut']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign[2]/bpel:copy/bpel:to[@variable='addressVariable']").getLength());
    }
    /**
     * 
     *
     */
    public final void testMixedInvoke() {
        
        //FIXME name attributes are removed from all activities but <scope>s
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:flow/bpel:invoke[@name='mixed']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='mixed' and @partnerLink = 'serverPartnerLink']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='mixed' and @operation = 'requestService']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='mixed' and @inputVariable = 'v0InputMessage']").getLength());
//        assertEquals(1, getNodes("//bpel:invoke[@name='mixed' and @outputVariable = 'v0OutputMessage']").getLength());
//                
//        // Testing scope
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed' and @suppressJoinFailure='yes']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:targets/bpel:joinCondition").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:targets/bpel:target[@linkName='ship-to-invoice']").getLength());
//
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']/bpel:transitionCondition").getLength());
//
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:faultHandlers/bpel:catch[@faultName='bpel:correlationViolation']/bpel:empty[@name='catch']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:faultHandlers/bpel:catchAll/bpel:empty[@name='catchAll']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:compensationHandler/bpel:empty[@name='compensationHandler']").getLength());
//
//        assertEquals(2, getNodes("//bpel:scope[@name='mixed']/bpel:flow/bpel:assign").getLength());
//        assertEquals(3, getNodes("//bpel:scope[@name='mixed']/bpel:flow/bpel:assign/bpel:copy").getLength());
//        
//        // Testing toParts
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='idVariable']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='v0InputMessage' and @part='id']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='descriptionVariable']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='mixed']/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='v0InputMessage' and @part='description']").getLength());
//
//        // Testing element part
//        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign[2]/bpel:copy/bpel:from[@variable='v0OutputMessage' and @part='addressOut']").getLength());
//        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:assign[2]/bpel:copy/bpel:to[@variable='addressVariable']").getLength());
     
        
    }
    
    
}
