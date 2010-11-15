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

        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:pick").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core' and @suppressJoinFailure = 'yes']/bpel:pick").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:pick[@createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:pick/bpel:onMessage[@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:pick/bpel:onMessage[@operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:pick/bpel:onMessage[@variable = 'request']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:pick/bpel:onMessage[@messageExchange = 'mea']").getLength());

        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:targets/bpel:target").getLength());

        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='core']/bpel:sources/bpel:source[@linkName='ship-to-scheduling']/bpel:transitionCondition").getLength());

    }


    /**
     * Testing
     */
    public final void testFromPartsPick() {

        assertEquals(1, getNodes("//bpel:flow[@name='fromparts']/bpel:pick").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='fromparts']/bpel:pick[@createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='fromparts']/bpel:pick/bpel:onMessage[not(bpel:fromParts)]").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='fromparts']/bpel:pick/bpel:onMessage[@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='fromparts']/bpel:pick/bpel:onMessage[@operation = 'requestService']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='fromparts']/bpel:pick/bpel:onMessage[@messageExchange = 'mec']").getLength());

        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:flow[@name='fromparts']/ancestor::bpel:scope[1]/bpel:variables/bpel:variable[@messageType='srv:requestMessage']").getLength());
        assertEquals(3, getNodes("//bpel:flow[@name='fromparts']/bpel:pick/bpel:onMessage/bpel:flow/*").getLength());

        // Check the assignments
        assertEquals(1, getNodes("//bpel:flow[@name='fromparts']/bpel:pick/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign").getLength());
        assertEquals(2, getNodes("//bpel:flow[@name='fromparts']/bpel:pick/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='fromparts']/bpel:pick/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:from[@part='id']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='fromparts']/bpel:pick/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='idVariable']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='fromparts']/bpel:pick/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:from[@part='description']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='fromparts']/bpel:pick/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='descriptionVariable' ]").getLength());

    }

    /**
     * 
     *
     */
    public final void testElementPick() {

        assertEquals(1, getNodes("//bpel:flow[@name='elements']/bpel:pick").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/bpel:pick[@createInstance = 'no']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/bpel:pick/bpel:onMessage[@partnerLink     = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/bpel:pick/bpel:onMessage[@operation       = 'simpleService']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/bpel:pick/bpel:onMessage[@messageExchange = 'med']").getLength());

        // Check the enclosing scope
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/ancestor::bpel:scope[1]/bpel:variables/bpel:variable[@messageType='srv:simpleRequestMessage']").getLength());
        assertEquals(3, getNodes("//bpel:flow[@name='elements']/bpel:pick/bpel:onMessage/bpel:flow/*").getLength());

        // Check the assignments
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/bpel:pick/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/bpel:pick/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/bpel:pick/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:from[@part='addressIn']").getLength());
        assertEquals(1, getNodes("//bpel:flow[@name='elements']/bpel:pick/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='addressVariable']").getLength());
    }

}
