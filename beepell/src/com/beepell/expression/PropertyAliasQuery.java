package com.beepell.expression;

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Node;

import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.SubLanguageExecutionFault;
import com.beepell.exceptions.UninitializedVariable;

/**
 * <p>
 * Property Aliases may contain a query. The result of the query is expected to
 * be of the type assigned in the Property. The property alias itself is also
 * typed to indicate which type of variables that the alias may apply.
 * <p>
 * The query expression is static and applied to different variables of the
 * indicated type. If this is a message type (which applies for all message
 * variables of that type) then a part MUST be specified. Type, part and
 * namespace context is also static.
 * 
 * @author Tim Hallwyl
 */
public class PropertyAliasQuery extends XPathExpression {

    private final QName type;

    private final String part;

    /**
     * Create a Property Alias Query Expression.
     * <p>
     * If expression is null, the context node is returned as if no query where
     * part of the alias.
     * 
     * @param type the message schema type
     * @param part the part of the message, that the query applies.
     * @param expression the query expression to apply.
     * @param namespaceContext the namespace context of the property alias query
     *            expression.
     */
    public PropertyAliasQuery(QName type, String part, String expression, NamespaceContext namespaceContext) {
        super(expression, namespaceContext);
        this.type = type;
        this.part = part;
    }

    /**
     * @param type the variable schema type
     * @param expression the query expression to apply.
     * @param namespaceContext the namespace context of the property alias query
     *            expression.
     */
    public PropertyAliasQuery(QName type, String expression, NamespaceContext namespaceContext) {
        super(expression, namespaceContext);
        this.type = type;
        this.part = null;
    }

    /**
     * @param variable Name of the variable to apply this query
     * @param part Name of message part. May be null, if a the variable is not a
     *            message type variable.
     * @param context The expression context
     * @return the Node selected by the query
     * @throws SubLanguageExecutionFault
     * @throws InvalidExpressionValue
     * @throws UninitializedVariable
     */
    public Node evaluate(String variable, ExpressionContext context) throws SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable {
        if (context.getVariableType(variable) == null)
            throw new InvalidExpressionValue("Variable '" + variable + "' was not found");

        if (!context.getVariableType(variable).equals(this.type))
            throw new InvalidExpressionValue("Variable '" + variable + "' is not of appropriate type: '" + this.type + "'.");

        // No variables and no functions
        XPathFactory factory = XPathFactory.newInstance();
        XPath xpath = factory.newXPath();
        xpath.setNamespaceContext(this.namespaceContext);

        Node contextNode;
        if (this.part == null)
            contextNode = context.getVariableValue(variable);
        else
            contextNode = context.getVariableValue(variable, this.part);

        if (expression == null)
            return contextNode;

        try {

            return (Node) xpath.evaluate(expression, contextNode, XPathConstants.NODE);

        } catch (XPathExpressionException exception) {
            throw new SubLanguageExecutionFault("Property Alias Query '" + expression + "' failed.", exception);
        }
    }

    /**
     * Used by MessageBroker to do correlation; when a message arrives it is not
     * yet in a variable, reachable though the ExecutionContext.
     * 
     * @param contextNode
     * @return the Node selected by the query
     * @throws SubLanguageExecutionFault
     * @throws InvalidExpressionValue
     * @throws UninitializedVariable
     */
    public Node evaluate(Node contextNode) throws SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable {
        // No variables and no functions
        XPathFactory factory = XPathFactory.newInstance();
        XPath xpath = factory.newXPath();
        xpath.setNamespaceContext(this.namespaceContext);

        if (expression == null)
            return contextNode;

        try {

            return (Node) xpath.evaluate(expression, contextNode, XPathConstants.NODE);

        } catch (XPathExpressionException exception) {
            throw new SubLanguageExecutionFault("Property Alias Query '" + expression + "' failed.", exception);
        }
    }

    /**
     * @return the part
     */
    public String getPart() {
        return part;
    }

    /**
     * @return the expression
     */
    public String getExpression() {
        return expression;
    }

}
