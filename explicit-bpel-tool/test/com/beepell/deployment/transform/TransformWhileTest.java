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

public class TransformWhileTest extends TestCase {

    private XPath xPath;
    private DocumentBuilder documentBuilder;
    private File file;

    @Override
    protected void setUp() throws Exception {
        super.setUp();

        this.file = new File("test/com/beepell/deployment/transform/while.bpel");

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
    public final void testWhile() throws Exception {
        Transform transfomer = new Transform();
        Document ebpel1 = transfomer.transform(file);
        assertNotNull(ebpel1);
    }
        
    public final void testPreConditions() throws Exception {
        String expression;
        Document document = documentBuilder.parse(file);

        expression = "//bpel:while/bpel:sources/bpel:source/bpel:transitionCondition/text()";
        assertEquals("true() or false()", (String) xPath.evaluate(expression, document, XPathConstants.STRING));

        expression = "//bpel:while/bpel:targets/bpel:joinCondition/text()";
        assertEquals("$l1 or not($l1)", (String) xPath.evaluate(expression, document, XPathConstants.STRING));

        expression = "//bpel:while/bpel:condition/text()";
        assertEquals("true()", (String) xPath.evaluate(expression, document, XPathConstants.STRING));
        
    }

    public final void testPostConditions() throws Exception {
        String expression;
        Transform transfomer = new Transform();
        Document ebpel = transfomer.transform(file);

        // 1. Test preservation of join condition
        expression = "//bpel:if/bpel:targets/bpel:joinCondition/text()";
        assertEquals("$l1 or not($l1)", (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING));

        // 2. Test preservation of transition condition
        expression = "//bpel:if/bpel:sources/bpel:source/bpel:transitionCondition/text()";
        assertEquals("true() or false()", (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING));
        
        // 3. Test if condition
        expression = "//bpel:if/bpel:condition/text()";
        assertEquals("true()", (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING));

        // 4. Test repeatUntil condition
        expression = "//bpel:repeatUntil/bpel:condition/text()";
        assertEquals("not(true())", (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING));

        // 5. Test preservation of enclosed activity
        expression = "count(//bpel:if/bpel:repeatUntil/bpel:scope[@name='empty'])";
        assertEquals(1, ((Double) xPath.evaluate(expression, ebpel, XPathConstants.NUMBER)).intValue());
        
        
    }
        

}
