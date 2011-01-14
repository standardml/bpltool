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
        assertEquals(1, getNodes("(//bpel:invoke)[1]/parent::bpel:flow[@suppressJoinFailure = 'yes']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[1][@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[1][@operation = 'requestService']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[1][@inputVariable = 'request']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[1][@outputVariable = 'response']").getLength());
        
        assertEquals(1, getNodes("(//bpel:invoke)[1]/parent::bpel:flow/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[1]/parent::bpel:flow/bpel:targets/bpel:target[@linkName='ship-to-invoice']").getLength());

        assertEquals(1, getNodes("(//bpel:invoke)[1]/parent::bpel:flow/bpel:sources/bpel:source[@linkName='ship-to-scheduling']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[1]/parent::bpel:flow/bpel:sources/bpel:source[@linkName='ship-to-scheduling']/bpel:transitionCondition").getLength());

        assertEquals(1, getNodes("(//bpel:invoke)[1]/bpel:correlations").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[1]/bpel:correlations/bpel:correlation[@set='PurchaseOrder' and @pattern='request']").getLength());
        
    }    
    
    
    
    /**
     * 
     *
     */
    public final void testLocalScope() {

        assertEquals(1, getNodes("(//bpel:invoke)[2]/parent::bpel:flow[@suppressJoinFailure='yes']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[2][@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[2][@operation = 'requestService']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[2][@inputVariable = 'request']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[2][@outputVariable = 'response']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[2]/bpel:correlations").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[2]/bpel:correlations/bpel:correlation[@set='PurchaseOrder' and @pattern='request']").getLength());
        
        assertEquals(1, getNodes("(//bpel:invoke)[2]/parent::bpel:flow/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[2]/parent::bpel:flow/bpel:targets/bpel:target[@linkName='ship-to-invoice']").getLength());

        assertEquals(1, getNodes("(//bpel:invoke)[2]/parent::bpel:flow/bpel:sources/bpel:source[@linkName='ship-to-scheduling']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[2]/parent::bpel:flow/bpel:sources/bpel:source[@linkName='ship-to-scheduling']/bpel:transitionCondition").getLength());

        assertEquals(1, getNodes("(//bpel:invoke)[2]/ancestor::bpel:scope[1]/bpel:faultHandlers/bpel:catch[@faultName='bpel:correlationViolation']/bpel:empty").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[2]/ancestor::bpel:scope[1]/bpel:faultHandlers/bpel:catchAll/bpel:empty").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[2]/ancestor::bpel:scope[1]/bpel:compensationHandler/bpel:empty").getLength());

    }
    
    /**
     * 
     *
     */
    public final void testToFromPartsInvoke() {
        
        // Check the invoke activity
        assertEquals(1, getNodes("(//bpel:invoke)[3]").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[3][@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[3][@operation = 'requestService']").getLength());

        assertEquals(1, getNodes("(//bpel:invoke)[3][not(bpel:toParts)]").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[3][not(bpel:fromParts)]").getLength());

        // Check the enclosing scope
        assertEquals(1, getNodes("(//bpel:invoke)[3]/ancestor::bpel:scope[1]/bpel:variables/bpel:variable[@messageType='srv:requestMessage']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[3]/ancestor::bpel:scope[1]/bpel:variables/bpel:variable[@messageType='srv:responseMessage']").getLength());
        assertEquals(4, getNodes("(//bpel:invoke)[3]/ancestor::bpel:scope[1]/bpel:flow/*").getLength());
        
        // Check the assignments
        assertEquals(2, getNodes("(//bpel:invoke)[3]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign").getLength());
        assertEquals(4, getNodes("(//bpel:invoke)[3]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[3]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:from[@variable='idVariable']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[3]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:to[@part='id']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[3]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:from[@variable='descriptionVariable']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[3]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:to[@part='description']").getLength());
        
        assertEquals(1, getNodes("(//bpel:invoke)[3]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[3]/bpel:assign/bpel:copy/bpel:from[@part='approved']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[3]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[3]/bpel:assign/bpel:copy/bpel:from[@part='note']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[3]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[3]/bpel:assign/bpel:copy/bpel:to[@variable='noteVariable' ]").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[3]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[3]/bpel:assign/bpel:copy/bpel:to[@variable='approvedVariable']").getLength());
        
        // Secondary tests: toParts alone
        assertEquals(1, getNodes("(//bpel:invoke)[4]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign").getLength());
        assertEquals(2, getNodes("(//bpel:invoke)[4]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[4]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:from[@variable='idVariable']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[4]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:to[@part='id']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[4]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:from[@variable='descriptionVariable']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[4]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:to[@part='description']").getLength());

        // Secondary tests: fromParts alone
        assertEquals(1, getNodes("(//bpel:invoke)[5]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[2]/bpel:assign").getLength());
        assertEquals(2, getNodes("(//bpel:invoke)[5]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[2]/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[5]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[2]/bpel:assign/bpel:copy/bpel:from[@part='approved']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[5]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[2]/bpel:assign/bpel:copy/bpel:to[@variable='approvedVariable']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[5]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[2]/bpel:assign/bpel:copy/bpel:from[@part='note']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[5]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[2]/bpel:assign/bpel:copy/bpel:to[@variable='noteVariable' ]").getLength());

                
    }
    
    /**
     * 
     *
     */
    public final void testElementsInvoke() {
        
        assertEquals(1, getNodes("(//bpel:invoke)[6]").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[6][@partnerLink   = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[6][@operation     = 'simpleService']").getLength());
        
        // Check the enclosing scope
        assertEquals(1, getNodes("(//bpel:invoke)[6]/ancestor::bpel:scope[1]/bpel:variables/bpel:variable[@messageType='srv:simpleRequestMessage']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[6]/ancestor::bpel:scope[1]/bpel:variables/bpel:variable[@messageType='srv:simpleResponseMessage']").getLength());
        assertEquals(4, getNodes("(//bpel:invoke)[6]/ancestor::bpel:scope[1]/bpel:flow/*").getLength());
        
        // Check the assignments
        assertEquals(2, getNodes("(//bpel:invoke)[6]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign").getLength());
        assertEquals(2, getNodes("(//bpel:invoke)[6]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[6]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:from[@variable='addressVariable']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[6]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:to[@part='addressIn']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[6]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[3]/bpel:assign/bpel:copy/bpel:from[@part='addressOut']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[6]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[3]/bpel:assign/bpel:copy/bpel:to[@variable='addressVariable']").getLength());
    }
    /**
     * 
     *
     */
    public final void testMixedInvoke() {
        
        assertEquals(1, getNodes("(//bpel:invoke)[7]").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[7][@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[7][@operation = 'requestService']").getLength());
                
        // Testing scope
        assertEquals(1, getNodes("(//bpel:invoke)[7]/parent::bpel:flow[@suppressJoinFailure='yes']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[7]/parent::bpel:flow/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[7]/parent::bpel:flow/bpel:targets/bpel:target[@linkName='ship-to-invoice']").getLength());

        assertEquals(1, getNodes("(//bpel:invoke)[7]/parent::bpel:flow/bpel:sources/bpel:source[@linkName='ship-to-scheduling']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[7]/parent::bpel:flow/bpel:sources/bpel:source[@linkName='ship-to-scheduling']/bpel:transitionCondition").getLength());

        assertEquals(1, getNodes("(//bpel:invoke)[7]/ancestor::bpel:scope[1]/bpel:faultHandlers/bpel:catch[@faultName='bpel:correlationViolation']/bpel:empty").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[7]/ancestor::bpel:scope[1]/bpel:faultHandlers/bpel:catchAll/bpel:empty").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[7]/ancestor::bpel:scope[1]/bpel:compensationHandler/bpel:empty").getLength());

        assertEquals(2, getNodes("(//bpel:invoke)[7]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign").getLength());
        assertEquals(3, getNodes("(//bpel:invoke)[7]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow/bpel:assign/bpel:copy").getLength());
        
        // Testing toParts
        assertEquals(1, getNodes("(//bpel:invoke)[7]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:from[@variable='idVariable']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[7]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:to[@part='id']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[7]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:from[@variable='descriptionVariable']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[7]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[1]/bpel:assign/bpel:copy/bpel:to[@part='description']").getLength());

        // Testing element part
        assertEquals(1, getNodes("(//bpel:invoke)[7]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[3]/bpel:assign/bpel:copy/bpel:from[@part='approved']").getLength());
        assertEquals(1, getNodes("(//bpel:invoke)[7]/ancestor::bpel:scope[1]/bpel:flow/bpel:flow[3]/bpel:assign/bpel:copy/bpel:to[@variable='addressVariable']").getLength());
     
        
    }
    
    
}
