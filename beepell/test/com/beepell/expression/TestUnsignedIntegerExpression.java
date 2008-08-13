package com.beepell.expression;

import java.io.File;

import javax.xml.namespace.NamespaceContext;

import junit.framework.TestCase;

import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * Test Unsigned Integer Expression.
 */
public class TestUnsignedIntegerExpression extends TestCase {

    /**
     * Test Unsigned Integer Expression.
     */
    public final void testExpressions() {
        try {
            ExpressionContext context = new TestExpressionContext();
            NamespaceContext nsc = new DocumentNamespaceContext(new File("test/com/beepell/expression/document.xml"));
            UnsignedIntegerExpression expression;

            expression = new UnsignedIntegerExpression("$person/dmy:age", nsc);
            assertEquals(new Long(86), expression.evaluate(context));


        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception.");
        }
    }
}
