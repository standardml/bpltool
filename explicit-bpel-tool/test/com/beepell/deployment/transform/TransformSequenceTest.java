package com.beepell.deployment.transform;
import java.io.File;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.beepell.deployment.transform.Transform;
import com.beepell.xml.namespace.DocumentNamespaceContext;

public class TransformSequenceTest extends TestCase {

    private XPath xPath;
    private DocumentBuilder documentBuilder;
    private File sequence1;
    private File sequence2;

    @Override
    protected void setUp() throws Exception {
        super.setUp();

        this.sequence1 = new File("test/com/beepell/deployment/transform/sequence.1.bpel");
        this.sequence2 = new File("test/com/beepell/deployment/transform/sequence.2.bpel");

        XPathFactory factory = XPathFactory.newInstance();
        this.xPath = factory.newXPath();
        this.xPath.setNamespaceContext(new DocumentNamespaceContext(sequence2));

        DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(true);
        this.documentBuilder = documentBuilderFactory.newDocumentBuilder();

    }

    /**
     * Test that the echo.bpel file is transformable.
     * <p>
     * This test will transform the echo.bpel from WS-BPEL standard syntax into
     * Core BPEL syntax. This will indirectly validate for WS-BPEL schema
     * compliance of the source and Core BPEL compliance of the resulting document.
     * 
     * @throws Exception if anything goes wrong.
     */
    public final void testSequence1() throws Exception {
        Transform transfomer = new Transform();
        Document ebpel1 = transfomer.transform(sequence1);
        assertNotNull(ebpel1);
    }

    public final void testSequence2() throws Exception {
        Transform transfomer = new Transform();
        Document ebpel2 = transfomer.transform(sequence2);
        assertNotNull(ebpel2);

    }

    
    public final void testPreConditions1() throws Exception {
        String expression;
        Document document = documentBuilder.parse(sequence1);

        expression = "//bpel:assign[@name='assign']/bpel:sources/bpel:source/bpel:transitionCondition/text()";
        assertEquals("true() or false()", (String) xPath.evaluate(expression, document, XPathConstants.STRING));

        expression = "//bpel:reply[@name='reply']/bpel:targets/bpel:joinCondition/text()";
        assertEquals("$reply or not($reply)", (String) xPath.evaluate(expression, document, XPathConstants.STRING));
        
        expression = "//bpel:empty[@name='empty1']/bpel:targets/bpel:joinCondition/text()";
        assertEquals("$empty or not($empty)", (String) xPath.evaluate(expression, document, XPathConstants.STRING));
        
    }
        
    public final void testPreConditions2() throws Exception {
        Document document = documentBuilder.parse(sequence2);
        String expression = "count(bpel:process/bpel:sequence[@name='s1']/bpel:sequence[@name='s1.3']/bpel:sequence[@name='s1.3.1']/bpel:sequence[@name='s1.3.1.1']/bpel:empty)";
        Double count = (Double) xPath.evaluate(expression, document, XPathConstants.NUMBER);
        assertEquals(4, count.intValue());
    }

    public final void testPostConditions1() throws Exception {
        String expression;
        Transform transfomer = new Transform();
        Document ebpel = transfomer.transform(sequence1);

        // 1. Test preservation of join condition in non-sequenced activity
        expression = "//bpel:flow[@name='reply']//bpel:reply/bpel:targets/bpel:joinCondition/text()";
        assertEquals("$reply or not($reply)", (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING));

        // 2. Test preservation of join condition in sequenced activity
        expression = "//bpel:flow[@name='empty1']/bpel:targets/bpel:joinCondition/text()";
        assertEquals("$empty or not($empty)", (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING));
        
        // 3. Test preservation of an existing target
        expression = "count(//bpel:flow[@name='empty1']/bpel:targets/bpel:target[@linkName='empty'])";
        assertEquals(1, ((Double) xPath.evaluate(expression, ebpel, XPathConstants.NUMBER)).intValue());
        
        expression = "count(//bpel:flow[@name='reply']/bpel:targets/bpel:target[@linkName='reply'])";
        assertEquals(1, ((Double) xPath.evaluate(expression, ebpel, XPathConstants.NUMBER)).intValue());
        
        // 4. Test preservation of a source
        expression = "//bpel:flow[@name='assign']/bpel:sources/bpel:source/bpel:transitionCondition/text()";
        assertEquals("true() or false()", (String) xPath.evaluate(expression, ebpel, XPathConstants.STRING));
        
        // 5. Test the extension of sources
        expression = "count(//bpel:flow[@name='receive']/bpel:sources/bpel:source)";
        assertEquals(1, ((Double) xPath.evaluate(expression, ebpel, XPathConstants.NUMBER)).intValue());

        expression = "count(//bpel:flow[@name='assign']/bpel:sources/bpel:source)";
        assertEquals(1, ((Double) xPath.evaluate(expression, ebpel, XPathConstants.NUMBER)).intValue());

        
    }
        
    public final void testPostConditions2() throws Exception {
        Transform transfomer = new Transform();
        Document ebpel = transfomer.transform(sequence2);
        String expression;

        // 1. Sequences at all levels are transformed into flow
        expression = "count(//bpel:flow[@name='s1']/bpel:flow/bpel:flow[@name='s1.3']/bpel:flow[@name='s1.3.1']/bpel:flow[@name='s1.3.1.1']/bpel:flow/bpel:empty)";
        Double count = (Double) xPath.evaluate(expression, ebpel, XPathConstants.NUMBER);
        assertEquals(4, count.intValue());

        // 2. The inner most flow declares tree links
        expression = "//bpel:flow[@name='s1.3.1.1']/bpel:links/bpel:link";
        NodeList links = (NodeList) xPath.evaluate(expression, ebpel, XPathConstants.NODESET);
        assertEquals(3, links.getLength());

        // 3. The first child of the inner most flow has a source link
        expression = "//bpel:flow[@name='s1.3.1.1']/bpel:flow[1]/bpel:sources/bpel:source";
        NodeList sources = (NodeList) xPath.evaluate(expression, ebpel, XPathConstants.NODESET);
        assertEquals(1, sources.getLength());

        // 4. The second child of the inner most flow has a source link
        expression = "//bpel:flow[@name='s1.3.1.1']/bpel:flow[2]/bpel:sources/bpel:source";
        sources = (NodeList) xPath.evaluate(expression, ebpel, XPathConstants.NODESET);
        assertEquals(1, sources.getLength());

        // 5. The second child of the inner most flow has a target link
        expression = "//bpel:flow[@name='s1.3.1.1']/bpel:flow[2]/bpel:targets/bpel:target";
        NodeList targets = (NodeList) xPath.evaluate(expression, ebpel, XPathConstants.NODESET);
        assertEquals(1, targets.getLength());
        
        // 6. The outer most flow has two links
        expression = "//bpel:flow[@name='s1']/bpel:links/bpel:link";
        links = (NodeList) xPath.evaluate(expression, ebpel, XPathConstants.NODESET);
        assertEquals(2, links.getLength());

        // 7. The first child of the outer most flow has a source link
        expression = "//bpel:flow[@name='s1']/bpel:flow[1]/bpel:sources/bpel:source";
        sources = (NodeList) xPath.evaluate(expression, ebpel, XPathConstants.NODESET);
        assertEquals(1, sources.getLength());
        
        // 8. The second child of the outer most flow has a source link
        expression = "//bpel:flow[@name='s1']/bpel:flow[2]/bpel:sources/bpel:source";
        sources = (NodeList) xPath.evaluate(expression, ebpel, XPathConstants.NODESET);
        assertEquals(1, sources.getLength());

        // 9. The second child of the outer most flow has a target link
        expression = "//bpel:flow[@name='s1']/bpel:flow[2]/bpel:targets/bpel:target";
        targets = (NodeList) xPath.evaluate(expression, ebpel, XPathConstants.NODESET);
        assertEquals(1, targets.getLength());
        
        // 9. The third child of the outer most flow has a target link
        expression = "//bpel:flow[@name='s1']/bpel:flow[3]/bpel:targets/bpel:target";
        targets = (NodeList) xPath.evaluate(expression, ebpel, XPathConstants.NODESET);
        assertEquals(1, targets.getLength());
        
        // 10. s1.1 has one link
        expression = "//bpel:flow[@name='s1.1']/bpel:links/bpel:link";
        links = (NodeList) xPath.evaluate(expression, ebpel, XPathConstants.NODESET);
        assertEquals(1, links.getLength());
        
    }

}
