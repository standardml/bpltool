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
public class TransformReceive2Test extends TestCase {

    private Document transformed;
    private XPath xPath = XPathFactory.newInstance().newXPath();

    @Override
    protected void setUp() throws Exception {
        super.setUp();

        File source = new File("test/com/beepell/deployment/transform/receive2.bpel");
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

    private String getString(String path) {
        try {
            return (String) xPath.evaluate(path, transformed, XPathConstants.STRING);
        } catch (XPathExpressionException e) {
            e.printStackTrace();
        }
        return null;
    }
    
    /**
     * Testing 
     */
    public final void testPostReceive() {
        // Attributes
        assertEquals("yes", getString("//bpel:pick/@createInstance"));
        assertEquals("receive", getString("//bpel:pick/@name"));
        
        // Links
        assertEquals("link1", getString("//bpel:pick/bpel:sources/bpel:source/@linkName"));
        
        // onMessage attributes
        assertEquals("echoPartnerLink", getString("//bpel:pick/bpel:onMessage/@partnerLink"));
        assertEquals("echoOperation", getString("//bpel:pick/bpel:onMessage/@operation"));
        assertEquals("requestMessage", getString("//bpel:pick/bpel:onMessage/@variable"));
        assertEquals("main", getString("//bpel:pick/bpel:onMessage/@messageExchange"));
        
        // onMessage correlation
        assertEquals("order-invoice", getString("//bpel:pick/bpel:onMessage/bpel:correlations/bpel:correlation/@set"));
        assertEquals("yes", getString("//bpel:pick/bpel:onMessage/bpel:correlations/bpel:correlation/@initiate"));
        
        // onMessage activity
        assertEquals(1, getNodes("//bpel:pick/bpel:onMessage/bpel:empty").getLength());
    }    


}
