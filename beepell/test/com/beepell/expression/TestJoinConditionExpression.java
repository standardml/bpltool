package com.beepell.expression;

import java.io.File;

import javax.xml.namespace.NamespaceContext;

import junit.framework.TestCase;

import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * Test Join Condition Expression
 */
public class TestJoinConditionExpression extends TestCase {

    /**
     * Test Join Condition Expression
     */
    public final void testExpressions() {
        try {
            ExpressionContext context = new TestExpressionContext();
            NamespaceContext nsc = new DocumentNamespaceContext(new File("test/com/beepell/expression/document.xml"));
            JoinConditionExpression expression;

            expression = new JoinConditionExpression("$tlink and $flink", nsc);
            assertEquals(false, expression.evaluate(context).booleanValue());

            expression = new JoinConditionExpression("$tlink or $flink", nsc);
            assertEquals(true, expression.evaluate(context).booleanValue());

            expression = new JoinConditionExpression("not($tlink)", nsc);
            assertEquals(false, expression.evaluate(context).booleanValue());

            expression = new JoinConditionExpression("not($flink)", nsc);
            assertEquals(true, expression.evaluate(context).booleanValue());

        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception.");
        }
    }

}
