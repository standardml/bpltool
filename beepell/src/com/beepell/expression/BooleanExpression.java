package com.beepell.expression;

import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;

import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.SubLanguageExecutionFault;

/**
 * Expression used in transition, while, and if conditions.
 * 
 * @author Tim Hallwyl
 */
public class BooleanExpression extends XPathExpression {

    /**
     * Create a Boolean Expression.
     * 
     * @param expression
     * @param namespaceContext
     */
    public BooleanExpression(String expression, NamespaceContext namespaceContext) {
        super(expression, namespaceContext);
    }

    /**
     * Evaluates the expression against the context.
     * 
     * @param context
     * @return the boolean value of the expression
     * @throws SubLanguageExecutionFault if expression is invalid or
     *             otherwise could not be evaluated.
     * @throws InvalidExpressionValue if the expression does not yield
     *             a boolean value.
     */
    public Boolean evaluate(ExpressionContext context) throws SubLanguageExecutionFault, InvalidExpressionValue {
        try {
            XPath xpath = getXPath(context);
            return (Boolean) xpath.evaluate(expression, document, XPathConstants.BOOLEAN);
        } catch (XPathExpressionException exception) {
            throw new SubLanguageExecutionFault("Boolean Expression '" + expression + "' failed.", exception);
        }
    }

}
