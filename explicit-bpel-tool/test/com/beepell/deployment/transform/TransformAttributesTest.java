package com.beepell.deployment.transform;

import java.io.File;

import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import junit.framework.TestCase;

import org.w3c.dom.Document;

import com.beepell.xml.namespace.DocumentNamespaceContext;


/**
 *
 * @author Tim Hallwyl, tiha@itu.dk
 */
public class TransformAttributesTest extends TestCase {

    private Transform transform;
    private Document transformed;
    private XPath xPath = XPathFactory.newInstance().newXPath();
    
    /* 
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        File source = new File("test/com/beepell/deployment/transform/attributes.bpel");
        this.transform = new Transform();
        transformed = transform.transform(source);
        NamespaceContext namespaceContext = new DocumentNamespaceContext(transformed);
        xPath.setNamespaceContext(namespaceContext);
    }

    private String getString(String path) {
        try {
            return (String) xPath.evaluate(path, transformed, XPathConstants.STRING);
        } catch (XPathExpressionException e) {
            e.printStackTrace();
        }
        return null;
    }

    
    public final void testExitOnStandardFault() {
        
        try {
            assertFalse((Boolean) xPath.evaluate("/bpel:process/@exitOnStandardFault", transformed, XPathConstants.BOOLEAN));
        } catch (XPathExpressionException e) {
            assertTrue(false);
        }
        assertEquals("yes", getString("//bpel:scope[@name='a']/@exitOnStandardFault"));
        assertEquals("no", getString("//bpel:scope[@name='b']/@exitOnStandardFault"));
        assertEquals("no", getString("//bpel:scope[@name='c']/@exitOnStandardFault"));
        
    }
    
    public final void testSuppressJoinFailure() {

        try {
            assertFalse((Boolean) xPath.evaluate("/bpel:process/@suppressJoinFailure", transformed, XPathConstants.BOOLEAN));
        } catch (XPathExpressionException e) {
            assertTrue(false);
        }
        assertEquals("yes", getString("//bpel:scope[@name='a']/bpel:flow/@suppressJoinFailure"));
        assertEquals("yes", getString("//bpel:scope[@name='a']/bpel:flow/bpel:flow[1]/@suppressJoinFailure"));
        assertEquals("yes", getString("//bpel:scope[@name='a']/bpel:flow/bpel:flow[2]/@suppressJoinFailure"));
        assertEquals("yes", getString("//bpel:scope[@name='b']/bpel:flow/@suppressJoinFailure"));
        assertEquals("no", getString("//bpel:scope[@name='b']/bpel:flow/bpel:flow[1]/@suppressJoinFailure"));
        assertEquals("no", getString("//bpel:scope[@name='b']/bpel:flow/bpel:flow[2]/@suppressJoinFailure"));
        assertEquals("yes", getString("//bpel:scope[@name='c']/bpel:flow/@suppressJoinFailure"));
        assertEquals("no", getString("//bpel:scope[@name='c']/bpel:flow/bpel:flow[1]/@suppressJoinFailure"));
        assertEquals("no", getString("//bpel:scope[@name='c']/bpel:flow/bpel:flow[2]/@suppressJoinFailure"));
        
    }
    
}
