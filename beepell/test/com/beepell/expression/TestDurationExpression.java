package com.beepell.expression;

import java.io.File;

import javax.xml.datatype.Duration;
import javax.xml.namespace.NamespaceContext;

import junit.framework.TestCase;

import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * Test Deadline Expressions.
 */
public class TestDurationExpression extends TestCase {

    /**
     * Test Deadline Expressions.
     */
    public final void testExpressions() {
        try {
            ExpressionContext context = new TestExpressionContext();
            NamespaceContext nsc = new DocumentNamespaceContext(new File("test/com/beepell/expression/document.xml"));
            DurationExpression expression;

            expression = new DurationExpression("$person/dmy:lease", nsc);
            Duration duration = expression.evaluate(context);
            assertEquals(5, duration.getYears());
            assertEquals(2, duration.getMonths());
            assertEquals(10, duration.getDays());
            assertEquals(15, duration.getHours());
            assertEquals(0, duration.getMinutes());


        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception.");
        }
    }
}
