package com.beepell.expression;

import java.io.File;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.NamespaceContext;

import junit.framework.TestCase;

import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * Test Deadline Expressions.
 * @author Tim Hallwyl
 *
 */
public class TestDeadlineExpression extends TestCase {
    /**
     * Test Deadline Expressions.
     */
    public final void testExpressions() {
        try {
            ExpressionContext context = new TestExpressionContext();
            NamespaceContext nsc = new DocumentNamespaceContext(new File("test/com/beepell/expression/document.xml"));
            DeadlineExpression expression;
            
            expression = new DeadlineExpression("$person/dmy:dateborn", nsc);
            XMLGregorianCalendar calendar = expression.evaluate(context);
            assertEquals(9, calendar.getDay());
            assertEquals(1, calendar.getMonth());
            assertEquals(1921, calendar.getYear());
            
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception.");
        }
    }
}
