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
        
        assertEquals("yes", getString("/bpel:process/@exitOnStandardFault"));
        assertEquals("yes", getString("//bpel:scope[@name='a']/@exitOnStandardFault"));
        assertEquals("no", getString("//bpel:scope[@name='b']/@exitOnStandardFault"));
        assertEquals("no", getString("//bpel:scope[@name='c']/@exitOnStandardFault"));
        
    }
    
    public final void testSuppressJoinFailure() {

        assertEquals("yes", getString("/bpel:process/@suppressJoinFailure"));
        assertEquals("yes", getString("//bpel:scope[@name='a']/@suppressJoinFailure"));
        assertEquals("no", getString("//bpel:scope[@name='b']/@suppressJoinFailure"));
        assertEquals("no", getString("//bpel:scope[@name='c']/@suppressJoinFailure"));
        assertEquals("no", getString("//bpel:pick/@suppressJoinFailure"));
        assertEquals("yes", getString("//bpel:scope[@name='c']/bpel:assign/@suppressJoinFailure"));
        assertEquals("no", getString("//bpel:reply/@suppressJoinFailure"));
        
    }
    
}
