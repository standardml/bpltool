package com.beepell.expression;

import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.SubLanguageExecutionFault;

/**
 * Expression used in transition, while, and if conditions.
 * 
 * @author Tim Hallwyl
 */
public class JoinConditionExpression extends XPathExpression {

    /**
     * Create a Join Condition Expression.
     * 
     * @param expression
     * @param namespaceContext
     */
    public JoinConditionExpression(String expression, NamespaceContext namespaceContext) {
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
        // Only link state as variables, no BPEL functions.
        XPathFactory factory = XPathFactory.newInstance();
        factory.setXPathVariableResolver(new LinkStateResolver(context));
        XPath xpath = factory.newXPath();
        xpath.setNamespaceContext(this.namespaceContext);
        try {
        
            return (Boolean) xpath.evaluate(expression, document, XPathConstants.BOOLEAN);
     
        } catch (XPathExpressionException exception) {
            throw new SubLanguageExecutionFault("Join Condition '" + expression + "' failed.", exception);
        }
    }

}
