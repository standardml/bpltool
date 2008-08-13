package com.beepell.expression;

import javax.xml.namespace.NamespaceContext;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.SubLanguageExecutionFault;

/**
 * General Expression used in &lt;assign&gt;.
 * 
 * @author Tim Hallwyl
 */
public class GeneralExpression extends XPathExpression {

    /**
     * Create a Boolean Expression.
     * 
     * @param expression
     * @param namespaceContext
     */
    public GeneralExpression(String expression, NamespaceContext namespaceContext) {
        super(expression, namespaceContext);
    }

    /**
     * Evaluates the expression against the context.
     * 
     * @param context
     * @return A W3C DOM Node, e.g. Text, Element or Attribute.
     * @throws SubLanguageExecutionFault if expression is invalid or
     *             otherwise could not be evaluated.
     * @throws InvalidExpressionValue if the expression does not yield
     *             a boolean value.
     */
    public Node evaluate(ExpressionContext context) throws SubLanguageExecutionFault {
        try {

            XPath xpath = getXPath(context);
            Node node;

            try {
                node = (Node) xpath.evaluate(expression, document, XPathConstants.NODE);
                return node;
            } catch (XPathExpressionException exception) {
                String text = (String) xpath.evaluate(expression, document, XPathConstants.STRING);
                Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
                Element element = document.createElementNS("http://beepell.com/expression/", "rvalue");
                node = document.createTextNode(text);
                element.appendChild(node);
                return node;
            }

        } catch (Exception exception) {
            throw new SubLanguageExecutionFault("General Expression '" + expression + "' failed: " + exception.getCause().getMessage(), exception);
        }
    }
}
