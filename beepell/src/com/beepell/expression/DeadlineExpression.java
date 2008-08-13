package com.beepell.expression;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;

import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.SubLanguageExecutionFault;

/**
 * Expression used in until expression of &lt;onAlarm&gt; and &lt;wait&gt;.
 * 
 * @author Tim Hallwyl
 */
public class DeadlineExpression extends XPathExpression {

    /**
     * Creates a DeadlineExpression.
     * 
     * @param expression
     * @param namespaceContext
     */
    public DeadlineExpression(String expression, NamespaceContext namespaceContext) {
        super(expression, namespaceContext);
    }

    /**
     * Evaluates the expression against the context.
     * 
     * @param context
     * @return calendar object representing the deadline.
     * @throws SubLanguageExecutionFault if expression is invalid or
     *             otherwise could not be evaluated.
     * @throws InvalidExpressionValue if the expression does not yield
     *             a boolean value.
     */
    public XMLGregorianCalendar evaluate(ExpressionContext context) throws SubLanguageExecutionFault, InvalidExpressionValue {
        String deadline = null;
        try {
            XPath xpath = getXPath(context);
            deadline = (String) xpath.evaluate(expression, document, XPathConstants.STRING);
            return datatypeFactory.newXMLGregorianCalendar(deadline);
        
        } catch (IllegalArgumentException exception) {
            throw new InvalidExpressionValue("The XPath string '" + deadline + "' is not a valid deadline.");
        } catch (Exception exception) {
            throw new SubLanguageExecutionFault("Dead-line expression '" + expression + " failed.", exception);
        }
    }
}
