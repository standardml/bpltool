package com.beepell.expression;

import java.io.File;

import javax.xml.namespace.NamespaceContext;

import junit.framework.TestCase;

import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * Tests BooleanExpression
 * 
 * @author Tim Hallwyl
 */
public class TestBooleanExpression extends TestCase {

    /**
     * Test Boolean Expressions.
     */
    public final void testExpressions() {
        try {
            ExpressionContext context = new TestExpressionContext();
            NamespaceContext nsc = new DocumentNamespaceContext(new File("test/com/beepell/expression/document.xml"));
            BooleanExpression expression;
            
            expression = new BooleanExpression("$tBln and $fBln", nsc);
            assertEquals(false, expression.evaluate(context).booleanValue());
            
            expression = new BooleanExpression("$tBln or $fBln", nsc);
            assertEquals(true, expression.evaluate(context).booleanValue());
            
            expression = new BooleanExpression("not($tBln)", nsc);
            assertEquals(false, expression.evaluate(context).booleanValue());
   
            expression = new BooleanExpression("not($fBln)", nsc);
            assertEquals(true, expression.evaluate(context).booleanValue());
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception.");
        }
    }
}
