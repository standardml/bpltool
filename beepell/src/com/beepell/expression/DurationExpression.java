package com.beepell.expression;

import javax.xml.datatype.Duration;
import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;

import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.SubLanguageExecutionFault;

/**
 * for expression of &lt;onAlarm&gt; and &lt;wait&gt;, &lt;repeatEvery&gt; expression of
 * &lt;onAlarm&gt;
 * 
 * @author Tim Hallwyl
 */
public class DurationExpression extends XPathExpression {

    /**
     * Creates a Duration Expression
     * @param expression
     * @param namespaceContext
     */
    public DurationExpression(String expression, NamespaceContext namespaceContext) {
        super(expression, namespaceContext);
    }
    
    /**
     * Evaluates the expression against the context.
     * 
     * @param context
     * @return Duration object representing the time period.
     * @throws SubLanguageExecutionFault if expression is invalid or
     *             otherwise could not be evaluated.
     * @throws InvalidExpressionValue if the expression does not yield
     *             a boolean value.
     */
    public Duration evaluate(ExpressionContext context) throws SubLanguageExecutionFault, InvalidExpressionValue {
        String duration = null;
        try {
            XPath xpath = getXPath(context);
            duration = (String) xpath.evaluate(expression, document, XPathConstants.STRING);
            return datatypeFactory.newDuration(duration);
            
        } catch (IllegalArgumentException exception) {
            throw new InvalidExpressionValue("The XPath string '" + duration + "' is not a valid duration.");
        } catch (XPathExpressionException exception) {
            throw new SubLanguageExecutionFault("Duration Expression '" + expression + "' failed.", exception);
        }
        
        
    }
}
