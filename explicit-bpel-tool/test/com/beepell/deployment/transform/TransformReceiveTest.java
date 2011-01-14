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

        assertEquals(1, getNodes("(//bpel:pick)[1]/parent::bpel:flow[@suppressJoinFailure = 'yes']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[1]").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[1][@createInstance = 'no']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[1]/bpel:onMessage").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[1]/bpel:onMessage[@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[1]/bpel:onMessage[@operation = 'requestService']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[1]/bpel:onMessage[@variable = 'request']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[1]/bpel:onMessage[@messageExchange = 'mea']").getLength());

        assertEquals(1, getNodes("(//bpel:pick)[1]/parent::bpel:flow/bpel:targets/bpel:joinCondition").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[1]/parent::bpel:flow/bpel:targets/bpel:target").getLength());

        assertEquals(1, getNodes("(//bpel:pick)[1]/parent::bpel:flow/bpel:sources/bpel:source[@linkName='ship-to-scheduling']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[1]/parent::bpel:flow/bpel:sources/bpel:source[@linkName='ship-to-scheduling']/bpel:transitionCondition").getLength());

    }

    /**
     * Testing
     */
    public final void testNovariableReceive() {

        assertEquals(1, getNodes("(//bpel:pick)[2][@createInstance = 'no']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[2]/bpel:onMessage[@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[2]/bpel:onMessage[@operation = 'noticeService']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[2]/bpel:onMessage[@messageExchange = 'meb']").getLength());
    }

    /**
     * Testing
     */
    public final void testFromPartsReceive() {

        assertEquals(1, getNodes("(//bpel:pick)[3]").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[3][@createInstance = 'no']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[3]/bpel:onMessage[not(bpel:fromParts)]").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[3]/bpel:onMessage[@partnerLink = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[3]/bpel:onMessage[@operation = 'requestService']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[3]/bpel:onMessage[@messageExchange = 'mec']").getLength());

        // Check the enclosing scope
        assertEquals(1, getNodes("(//bpel:pick)[3]/ancestor::bpel:scope[1]/bpel:variables/bpel:variable[@messageType='srv:requestMessage']").getLength());

        // Check the assignments
        assertEquals(1, getNodes("(//bpel:pick)[3]/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign").getLength());
        assertEquals(2, getNodes("(//bpel:pick)[3]/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[3]/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:from[@part='id']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[3]/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='idVariable']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[3]/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:from[@part='description']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[3]/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='descriptionVariable']").getLength());

    }

    /**
     * 
     *
     */
    public final void testElementReceive() {

        assertEquals(1, getNodes("(//bpel:pick)[4]").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[4][@createInstance = 'no']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[4]/bpel:onMessage[@partnerLink     = 'serverPartnerLink']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[4]/bpel:onMessage[@operation       = 'simpleService']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[4]/bpel:onMessage[@messageExchange = 'med']").getLength());

        // Check the enclosing scope
        assertEquals(1, getNodes("(//bpel:pick)[4]/ancestor::bpel:scope[1]/bpel:variables/bpel:variable[@messageType='srv:simpleRequestMessage']").getLength());

        // Check the assignments
        assertEquals(1, getNodes("(//bpel:pick)[4]/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[4]/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[4]/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:from[@part='addressIn']").getLength());
        assertEquals(1, getNodes("(//bpel:pick)[4]/bpel:onMessage/bpel:flow/bpel:flow/bpel:assign/bpel:copy/bpel:to[@variable='addressVariable']").getLength());
    }

}
