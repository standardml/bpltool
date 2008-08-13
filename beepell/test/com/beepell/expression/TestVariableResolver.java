package com.beepell.expression;

import java.io.File;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * Testing the VariableResolver
 * @author Tim Hallwyl
 *
 */
public class TestVariableResolver extends TestCase {

    /**
     * Test using real XPath expression and a fake expression context.
     *
     */
    public final void testExpressions() {
        
        
        try {
            String expression;
            ExpressionContext context = new TestExpressionContext();
            VariableResolver resolver = new VariableResolver(context);

            /*
             * http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6211561
             * As a workaround to bug 6211561, we need an empty document to
             * use as context node, when no context node is needed.
             */
            DocumentBuilderFactory dFactory = DocumentBuilderFactory.newInstance();
            dFactory.setValidating(false);
            dFactory.setNamespaceAware(true);
            DocumentBuilder dBuilder = dFactory.newDocumentBuilder();
            Document document = dBuilder.newDocument();
            
            XPathFactory factory = XPathFactory.newInstance();
            factory.setXPathVariableResolver(resolver);
            XPath xpath = factory.newXPath();
            xpath.setNamespaceContext(new DocumentNamespaceContext(new File("test/com/beepell/expression/document.xml")));

            
            // SIMPLE TYPES            
            expression = "$tBln";
            assertEquals(true, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));

            expression = "$fBln";
            assertEquals(false, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));
            
            expression = "not($tBln)";
            assertEquals(false, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));

            expression = "not($fBln)";
            assertEquals(true, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));

            expression = "$flt";
            assertEquals(1234, Math.round( 1000 * ((Double) xpath.evaluate(expression, (Node) null, XPathConstants.NUMBER) )));
            
            expression = "$ntgr";
            assertEquals(new Double(-42), xpath.evaluate(expression, (Node) null, XPathConstants.NUMBER));

            expression = "$untgr";
            assertEquals(new Double(42), xpath.evaluate(expression, (Node) null, XPathConstants.NUMBER));
            
            // COMPLEX TYPES
            expression = "local-name($personinfo)";
            assertEquals("employee", xpath.evaluate(expression, document, XPathConstants.STRING));

            expression = "$personinfo/dmy:firstname/text()";
            assertEquals("Simon", (String) xpath.evaluate(expression, document, XPathConstants.STRING));

            expression = "$personinfo/dmy:lastname/text()";
            assertEquals("Spies", (String) xpath.evaluate(expression, document, XPathConstants.STRING));

            // ELEMENT
            expression = "local-name($person)";
            assertEquals("person", xpath.evaluate(expression, document, XPathConstants.STRING));

            expression = "$person/dmy:firstname/text()";
            assertEquals("Simon", (String) xpath.evaluate(expression, document, XPathConstants.STRING));

            expression = "$person/dmy:lastname/text()";
            assertEquals("Spies", (String) xpath.evaluate(expression, document, XPathConstants.STRING));
            
            expression = "$person/dmy:age/text()";
            assertEquals(new Double(86), (Double) xpath.evaluate(expression, document, XPathConstants.NUMBER));
            
            expression = "$person/dmy:dateborn/text()";
            assertEquals("1921-01-09", (String) xpath.evaluate(expression, document, XPathConstants.STRING));
                        
            // MESSAGE TYPE
            expression = "$message.personinfo/dmy:firstname/text()";
            assertEquals("Simon", (String) xpath.evaluate(expression, document, XPathConstants.STRING));
            
            expression = "$message.person/dmy:firstname/text()";
            assertEquals("Simon", (String) xpath.evaluate(expression, document, XPathConstants.STRING));
           
            expression = "$message.tBln";
            assertEquals(true, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));

            expression = "$message.fBln";
            assertEquals(false, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));
            
            expression = "not($message.tBln)";
            assertEquals(false, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));

            expression = "not($message.fBln)";
            assertEquals(true, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));

            expression = "$message.flt";
            assertEquals(1234, Math.round( 1000 * ((Double) xpath.evaluate(expression, (Node) null, XPathConstants.NUMBER) )));
            
            expression = "$message.ntgr";
            assertEquals(new Double(-42), xpath.evaluate(expression, (Node) null, XPathConstants.NUMBER));

            expression = "$message.untgr";
            assertEquals(new Double(42), xpath.evaluate(expression, (Node) null, XPathConstants.NUMBER));
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception.");
        }
    }
    
}
