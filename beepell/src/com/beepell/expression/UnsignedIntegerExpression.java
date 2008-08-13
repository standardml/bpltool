package com.beepell.expression;

import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;

import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.SubLanguageExecutionFault;

/**
 * Expression used in &lt;startCounterValue&gt;, &lt;finalCounterValue&gt;, and
 * &lt;branches&gt; in &lt;forEach&gt;
 * 
 * @author Tim Hallwyl
 */
public class UnsignedIntegerExpression extends XPathExpression {
    
    
    /**
     * Create a Unsigned Integer Expression.
     * 
     * @param expression
     * @param namespaceContext
     */
    public UnsignedIntegerExpression(String expression, NamespaceContext namespaceContext) {
        super(expression, namespaceContext);
    }
    /**
     * Evaluates the expression against the context.
     * 
     * @param context
     * @return Long object representing the unsigned integer.
     * @throws SubLanguageExecutionFault if expression is invalid or
     *             otherwise could not be evaluated.
     * @throws InvalidExpressionValue if the expression does not yield
     *             a boolean value.
     */
    public Long evaluate(ExpressionContext context) throws SubLanguageExecutionFault, InvalidExpressionValue {
        try {
            XPath xpath = getXPath(context);
            return ((Double) xpath.evaluate(expression, document, XPathConstants.NUMBER)).longValue();
        } catch (XPathExpressionException exception) {
            throw new SubLanguageExecutionFault("Unsigned integer expression '" + expression + "' failed.", exception);
        }
    }

}
