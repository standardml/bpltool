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
 * This test is not transforming into true Core BPEL as is is testing the
 * intermediate core receive before this is transformed into a Pick activity.
 * 
 * @author Tim Hallwyl
 */
public class TransformReceiveTest extends TestCase {

    private Document transformed;
    private XPath xPath = XPathFactory.newInstance().newXPath();

    @Override
    protected void setUp() throws Exception {
        super.setUp();

        File source = new File("test/com/beepell/deployment/transform/receive.bpel");
        Transform transform = new Transform();
        /*
        transform.setSheets(new String[] { 
        		"process.xsl",                        // Move process scope to an explicit scope
        		"if.xsl",                             // Add missing else-clause and turn elseif into nested if
        		"while.xsl",                          // while to repeatUntil
        		"scope.xsl",                          // Variable initializations
        		"documentation.xsl",                  // remove human readable documantation
        		"extension-activity.xsl",             // remove extension activities
                "extension-assign.xsl",               // remove extension assignments
                "extension-attributes-elements.xsl",  // remove extension attributes and elements
                "extension-declarations.xsl",         // remove extension declarations
                "irra.xsl",                           // invoke.xsl, receive.xsl, reply.xsl, tofromparts.xsl
                // "receive2.xsl",                    // receive to pick
                "handlers.xsl", 
                "sequence.xsl", 
                "defaultconditions.xsl", 
                "attributes.xsl", 
                "defaults.xsl", 
                "language.xsl" }); */
        transform.setValidate(false);
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

        assertEquals(1, getNodes("//bpel:pick[@name='core']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core' and @suppressJoinFailure = 'yes']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core']/bpel:onMessage[@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core']/bpel:onMessage[@operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core']/bpel:onMessage[@variable = 'request']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core']/bpel:onMessage[@messageExchange = 'mea']").getLength());

        assertEquals(1, getNodes("//bpel:pick[@name='core']/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core']/bpel:targets/bpel:target").getLength());

        assertEquals(1, getNodes("//bpel:pick[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']/bpel:transitionCondition").getLength());

    }

    /**
     * Testing
     */
    public final void testNovariableReceive() {

        assertEquals(1, getNodes("//bpel:pick[@name='novariable']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='novariable' and @suppressJoinFailure = 'yes']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='novariable' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='novariable']/bpel:onMessage[@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='novariable']/bpel:onMessage[@operation = 'noticeService']").getLength());
        assertEquals(0, getNodes("//bpel:pick[@name='novariable']/bpel:onMessage[@variable]").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='novariable']/bpel:onMessage[@messageExchange = 'meb']").getLength());
    }

    /**
     * Testing
     */
    public final void testFromPartsReceive() {

        // Check the invoke activity
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:flow/bpel:pick[@name='fromparts']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='fromparts' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='fromparts' and not(bpel:fromParts)]").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='fromparts']/bpel:onMessage[@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='fromparts']/bpel:onMessage[@operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='fromparts']/bpel:onMessage[@variable = 'v0OutputMessage']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='fromparts']/bpel:onMessage[@messageExchange = 'mec']").getLength());

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

        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:flow/bpel:pick[@name='elements']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='elements' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='elements']/bpel:onMessage[@partnerLink   = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='elements']/bpel:onMessage[@operation     = 'simpleService']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='elements']/bpel:onMessage[@variable = 'v0OutputMessage']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='elements']/bpel:onMessage[@messageExchange = 'med']").getLength());

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
