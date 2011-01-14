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
    private File file;

    @Override
    protected void setUp() throws Exception {
        super.setUp();

        this.file = new File("test/com/beepell/deployment/transform/repeatUntil.bpel");

        XPathFactory factory = XPathFactory.newInstance();
        this.xPath = factory.newXPath();
        this.xPath.setNamespaceContext(new DocumentNamespaceContext(file));

        DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(true);
        this.documentBuilder = documentBuilderFactory.newDocumentBuilder();

    }

    /**
     * Test that the echo.bpel file is transformable.
     * <p>
     * This test will transform the file from WS-BPEL standard syntax into
     * Core BPEL syntax. This will indirectly validate for WS-BPEL schema
     * compliance of the source and Core BPEL compliance of the resulting document.
     * 
     * @throws Exception if anything goes wrong.
     */
    public final void testRepeatUntil() throws Exception {
        Transform transfomer = new Transform();
        Document ebpel1 = transfomer.transform(file);
        assertNotNull(ebpel1);
    }

    public final void testPreConditions() throws Exception {
        String expression;
        Document document = documentBuilder.parse(file);

        expression = "//bpel:repeatUntil/bpel:sources/bpel:source/bpel:transitionCondition/text()";
        assertEquals("true() or false()", (String) xPath.evaluate(expression, document, XPathConstants.STRING));

        expression = "//bpel:repeatUntil/bpel:targets/bpel:joinCondition/text()";
        assertEquals("$l1 or not($l1)", (String) xPath.evaluate(expression, document, XPathConstants.STRING));

        expression = "//bpel:repeatUntil/bpel:condition/text()";
        assertEquals("true()", (String) xPath.evaluate(expression, document, XPathConstants.STRING));
        
    }

    public final void testPostConditions() throws Exception {
        String expression;
        Transform transfomer = new Transform();
        Document ebpel = transfomer.transform(file);

        // Test preservation of join condition
        expression = "//bpel:scope[@name='repeatUntil']/parent::bpel:flow/bpel:targets/bpel:joinCondition/text()";
        assertEquals("$l1 or not($l1)", (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING));

        // Test preservation of transition condition
        expression = "//bpel:scope[@name='repeatUntil']/parent::bpel:flow/bpel:sources/bpel:source/bpel:transitionCondition/text()";
        assertEquals("true() or false()", (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING));
        
        // Test if condition
        expression = "//bpel:scope[@name='repeatUntil']/descendant::bpel:if/bpel:condition/text()";
        assertEquals("true()", (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING));

        // Test while condition
        //  get the condition variable name
        expression = "//bpel:scope[@name='repeatUntil']/ancestor::bpel:scope[bpel:variables][1]/bpel:variables/bpel:variable/@name";
        String tmpCondVar = (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING);
        expression = "normalize-space(//bpel:scope[@name='repeatUntil']/descendant::bpel:while/bpel:condition/text())";
        assertEquals("not($" + tmpCondVar + ")", (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING));

        // Test preservation of enclosed activity
        expression = "count(//bpel:scope[@name='repeatUntil']/descendant::bpel:while/bpel:flow/bpel:flow[1]/bpel:empty)";
        assertEquals(1, ((Double) xPath.evaluate(expression, ebpel, XPathConstants.NUMBER)).intValue());
        
        
    }
        

}
