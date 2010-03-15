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
 */
public class TransformPickTest extends TestCase {

    private Document transformed;
    private XPath xPath = XPathFactory.newInstance().newXPath();

    @Override
    protected void setUp() throws Exception {
        super.setUp();

        File source = new File("test/com/beepell/deployment/transform/pick.bpel");
        Transform transform = new Transform();
        transform.setValidate(false);
        //transform.setSheets(new String[]{"pick.xsl"});
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
    public final void testCorePick() {

        assertEquals(1, getNodes("//bpel:pick[@name='core']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core' and @suppressJoinFailure = 'yes']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core']/bpel:onMessage[@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core']/bpel:onMessage[@operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core']/bpel:onMessage[@variable = 'request']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='core']/bpel:onMessage[@messageExchange = 'mea']").getLength());

        assertEquals(1, getNodes("//bpel:scope[@name='core']/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='core']/bpel:targets/bpel:target").getLength());

        assertEquals(1, getNodes("//bpel:scope[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']/bpel:transitionCondition").getLength());

    }


    /**
     * Testing
     */
    public final void testFromPartsPick() {

        // Check the invoke activity
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:pick[@name='fromparts']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='fromparts' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='fromparts']/bpel:onMessage[not(bpel:fromParts)]").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='fromparts']/bpel:onMessage[@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='fromparts']/bpel:onMessage[@operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='fromparts']/bpel:onMessage[@variable = 'v01OutputMessage']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='fromparts']/bpel:onMessage[@messageExchange = 'mec']").getLength());

        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:variables/bpel:variable[@name = 'v01OutputMessage' and @messageType='srv:requestMessage']").getLength());
        assertEquals(3, getNodes("//bpel:scope[@name='fromparts']/bpel:pick[@name='fromparts']/bpel:onMessage/bpel:flow/*").getLength());

        // Check the assignments
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:pick[@name='fromparts']/bpel:onMessage/bpel:flow/bpel:assign").getLength());
        assertEquals(2, getNodes("//bpel:scope[@name='fromparts']/bpel:pick[@name='fromparts']/bpel:onMessage/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:pick[@name='fromparts']/bpel:onMessage/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='v01OutputMessage' and @part='id']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:pick[@name='fromparts']/bpel:onMessage/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='idVariable']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:pick[@name='fromparts']/bpel:onMessage/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='v01OutputMessage' and @part='description']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='fromparts']/bpel:pick[@name='fromparts']/bpel:onMessage/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='descriptionVariable' ]").getLength());

    }

    /**
     * 
     *
     */
    public final void testElementPick() {

        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:pick[@name='elements']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='elements' and @createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='elements']/bpel:onMessage[@partnerLink   = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='elements']/bpel:onMessage[@operation     = 'simpleService']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='elements']/bpel:onMessage[@variable = 'v01OutputMessage']").getLength());
        assertEquals(1, getNodes("//bpel:pick[@name='elements']/bpel:onMessage[@messageExchange = 'med']").getLength());

        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:variables/bpel:variable[@name = 'v01OutputMessage' and @messageType='srv:simpleRequestMessage']").getLength());
        assertEquals(3, getNodes("//bpel:scope[@name='elements']/bpel:pick[@name='elements']/bpel:onMessage/bpel:flow/*").getLength());

        // Check the assignments
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:pick[@name='elements']/bpel:onMessage/bpel:flow/bpel:assign").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:pick[@name='elements']/bpel:onMessage/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:pick[@name='elements']/bpel:onMessage/bpel:flow/bpel:assign/bpel:copy/bpel:from[@variable='v01OutputMessage' and @part='addressIn']").getLength());
        assertEquals(1, getNodes("//bpel:scope[@name='elements']/bpel:pick[@name='elements']/bpel:onMessage/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='addressVariable']").getLength());
    }

}
