package com.beepell.expression;

import java.io.File;

import javax.xml.namespace.NamespaceContext;

import junit.framework.TestCase;

import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import com.beepell.util.XML;
import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * Test General Expression.
 */
public class TestGeneralExpression extends TestCase {

    /**
     * Test General Expression.
     */
    public final void testExpressions() {
        try {
            ExpressionContext context = new TestExpressionContext();
            NamespaceContext nsc = new DocumentNamespaceContext(new File("test/com/beepell/expression/document.xml"));
            GeneralExpression expression;

            expression = new GeneralExpression("$person/dmy:lastname", nsc);
            Node node = expression.evaluate(context);
            if (node instanceof Element) {
                assertEquals("lastname", node.getLocalName());
                assertEquals("Spies", node.getTextContent());
            } else {
                fail("Expression did not return an Element node.");
            }
            
            expression = new GeneralExpression("number($person/dmy:age) * 0.8", nsc);
            node = expression.evaluate(context);
            if (node instanceof Text) {
                assertEquals("68.8", node.getTextContent());
                assertEquals("rvalue", node.getParentNode().getLocalName());
            } else {
                fail("Expression did not return an Text node.");
            }

            expression = new GeneralExpression("string($person/dmy:firstname)", nsc);
            node = expression.evaluate(context);
            if (node instanceof Text) {
                assertEquals("Simon", node.getTextContent());
                assertEquals("rvalue", node.getParentNode().getLocalName());
            } else {
                fail("Expression did not return an Text node.");
            }
            
            expression = new GeneralExpression("false()", nsc);
            node = expression.evaluate(context);
            if (node instanceof Text) {
                assertEquals("false", node.getTextContent());
                assertEquals("rvalue", node.getParentNode().getLocalName());
            } else {
                fail("Expression did not return an Text node.");
            }

            
            expression = new GeneralExpression("$person/dmy:firstname/text()", nsc);
            node = expression.evaluate(context);
            System.out.println(XML.toString(node));
            
            if (node instanceof Text) {
                assertEquals("Simon", node.getTextContent());
                assertEquals("firstname", node.getParentNode().getLocalName());
            } else {
                fail("Expression did not return an Text node.");
            }
            
            expression = new GeneralExpression("$person/@sex", nsc);
            node = expression.evaluate(context);
            System.out.println(XML.toString(node));
            
            if (node instanceof Attr) {
                assertEquals("male", node.getTextContent());
            } else {
                fail("Expression did not return an Text node.");
            }
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception.");
        }
    }
}
