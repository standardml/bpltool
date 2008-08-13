package com.beepell.expression;

import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Node;

import junit.framework.TestCase;

/**
 * Testing LinkStateResolver
 * @author Tim Hallwyl
 *
 */
public class TestLinkStateResolver extends TestCase {

    /**
     * Test using real XPath expression and a fake expression context.
     *
     */
    public final void testExpressions() {

        String expression;
        ExpressionContext context = new TestExpressionContext();
        LinkStateResolver linkStateResolver = new LinkStateResolver(context);

        XPathFactory factory = XPathFactory.newInstance();
        factory.setXPathVariableResolver(linkStateResolver);
        XPath xpath = factory.newXPath();

        try {
            expression = "$tlink and $flink";
            assertEquals(false, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));

            expression = "$tlink or $flink";
            assertEquals(true, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));

            expression = "$tlink and not($flink)";
            assertEquals(true, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));

            expression = "not($tlink) or $flink";
            assertEquals(false, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));

            expression = "$tlink";
            assertEquals(true, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));

            expression = "$flink";
            assertEquals(false, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception.");
        }

        /*
         * When referring an unset or unknown link, an exception must be thrown.
         */
        try {
            expression = "$xlink";
            assertEquals(true, xpath.evaluate(expression, (Node) null, XPathConstants.BOOLEAN));
            fail("Expected exception not thrown.");
        } catch (Exception exception) {
            // Exception expected
        }
        
        
    }

}
