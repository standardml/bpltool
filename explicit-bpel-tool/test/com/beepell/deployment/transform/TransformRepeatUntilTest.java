package com.beepell.deployment.transform;
import java.io.File;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import junit.framework.TestCase;

import org.w3c.dom.Document;

import com.beepell.xml.namespace.DocumentNamespaceContext;

public class TransformRepeatUntilTest extends TestCase {

    private XPath xPath;
    private DocumentBuilder documentBuilder;
    private File repeatUntil;

    @Override
    protected void setUp() throws Exception {
        super.setUp();

        this.repeatUntil = new File("test/com/beepell/deployment/transform/repeatUntil.bpel");

        XPathFactory factory = XPathFactory.newInstance();
        this.xPath = factory.newXPath();
        this.xPath.setNamespaceContext(new DocumentNamespaceContext(this.repeatUntil));

        DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(true);
        this.documentBuilder = documentBuilderFactory.newDocumentBuilder();

    }

    /**
     * Test that the repeatUntil.bpel file is transformable.
     * <p>
     * This test will transform the echo.bpel from WS-BPEL standard syntax into
     * E-BPEL syntax. This will indirectly validate for WS-BPEL schema
     * compliance of the source and E-BPEL compliance of the resulting document.
     * 
     * @throws Exception if anything goes wrong.
     */
    public final void testRepeatUntilTransformation() throws Exception {
        Transform transfomer = new Transform();
        Document ebpel1 = transfomer.transform(repeatUntil);
        assertNotNull(ebpel1);
    }

    public final void testPreConditions1() throws Exception {
        String expression;
        Document document = documentBuilder.parse(repeatUntil);

        expression = "//bpel:repeatUntil[@name='repeatUntil']/bpel:sources/bpel:source/@linkName";
        assertEquals("l1", (String) xPath.evaluate(expression, document, XPathConstants.STRING));

        expression = "//bpel:repeatUntil[@name='repeatUntil']/bpel:condition/text()";
        assertEquals("true()", (String) xPath.evaluate(expression, document, XPathConstants.STRING));
        
        expression = "count(//bpel:scope[@name='empty']/bpel:empty)";
        assertEquals(1d, (Double) xPath.evaluate(expression, document, XPathConstants.NUMBER));
        
    }

    public final void testPostConditions1() throws Exception {
        String expression;
        Transform transfomer = new Transform();
        Document ebpel = transfomer.transform(repeatUntil);

        // 1. Test conversion into sequence (which is transformed into a flow)
        expression = "//bpel:flow[@name='repeatUntil']/@suppressJoinFailure";
        assertEquals("yes", (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING));

        // 2. Test for copy of enclosed activity
        expression = "count(//bpel:flow[@name='repeatUntil']/bpel:scope)";
        assertEquals(1d, (Double) xPath.evaluate(expression, ebpel, XPathConstants.NUMBER));

        // 3. Test for keeping enclosed activity
        expression = "count(//bpel:flow[@name='repeatUntil']/bpel:while/bpel:scope)";
        assertEquals(1d, (Double) xPath.evaluate(expression, ebpel, XPathConstants.NUMBER));

        // 4. Test for negation of condition
        expression = "//bpel:flow[@name='repeatUntil']/bpel:while/bpel:condition/text()";
        assertEquals("not(true())", (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING));

        
    }
        

}
