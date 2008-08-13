package com.beepell.expression;

import javax.xml.namespace.NamespaceContext;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.SubLanguageExecutionFault;
import com.beepell.exceptions.UninitializedVariable;


/**
 * @author Tim Hallwyl
 *
 */
public class CopyQuery extends XPathExpression {
    
    /**
     * Create a Copy Query Expression.
     * 
     * @param expression
     * @param namespaceContext
     */
    public CopyQuery(String expression, NamespaceContext namespaceContext) {
        super(expression, namespaceContext);
    }
    
    /**
     * 
     * @param variable Name of the variable to apply this query
     * @param part Name of message part. May be null, if a the variable is not a message type variable.
     * @param context The expression context
     * @return the Node selected by the query
     * @throws SubLanguageExecutionFault
     * @throws InvalidExpressionValue
     * @throws UninitializedVariable 
     */
    public Node evaluate(String variable, String part, ExpressionContext context) throws SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable {
        try {
            XPath xpath = getXPath(context);
            Node contextNode;
            
            if (part == null)
                contextNode = context.getVariableValue(variable);
            else
                contextNode = context.getVariableValue(variable, part);
                
            Node node;
            try {
                node = (Node) xpath.evaluate(expression, contextNode, XPathConstants.NODE);
                return node;
            } catch (XPathExpressionException exception) {
                String text = (String) xpath.evaluate(expression, contextNode, XPathConstants.STRING);
                Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
                Element element = document.createElementNS("http://beepell.com/expression/", "rvalue");
                node = document.createTextNode(text);
                element.appendChild(node);
                return node;
            }
            
        } catch (XPathExpressionException exception) {
            throw new SubLanguageExecutionFault("Copy Query '" + expression + "' failed.", exception);
        } catch (ParserConfigurationException exception) {
            /* Should not happen */
            throw new SubLanguageExecutionFault(exception); 
        }
    }
    
    /**
     * 
     * @param variable Name of the variable to apply this query
     * @param context The expression context
     * @return the Node selected by the query
     * @throws SubLanguageExecutionFault
     * @throws InvalidExpressionValue
     * @throws UninitializedVariable 
     */
    public Node evaluate(String variable, ExpressionContext context) throws SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable {
        return this.evaluate(variable, null, context);
    }
    
}
