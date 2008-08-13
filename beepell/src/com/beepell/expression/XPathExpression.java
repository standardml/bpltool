package com.beepell.expression;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.NamespaceContext;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;

abstract class XPathExpression {

    protected static DatatypeFactory datatypeFactory;

    protected final NamespaceContext namespaceContext;

    protected final String expression;

    /**
     * http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6211561 As a
     * workaround to bug 6211561, we need an empty document to use as context
     * node, when no context node is needed.
     */
    protected static Document document;

    protected XPathExpression(String expression, NamespaceContext namespaceContext) {
        this.expression = expression;
        this.namespaceContext = namespaceContext;

        try {
            if (datatypeFactory == null)
                datatypeFactory = DatatypeFactory.newInstance();

            DocumentBuilderFactory dFactory = DocumentBuilderFactory.newInstance();
            dFactory.setValidating(false);
            dFactory.setNamespaceAware(true);
            DocumentBuilder dBuilder = dFactory.newDocumentBuilder();
            document = dBuilder.newDocument();

        } catch (ParserConfigurationException exception) {
            throw new IllegalStateException("Unable to get a document builder.", exception);
        } catch (DatatypeConfigurationException exception) {
            throw new IllegalStateException("Unable to get a datatype factory.", exception);
        }

    }

    protected XPath getXPath(ExpressionContext context) {
        XPathFactory factory = XPathFactory.newInstance();
        factory.setXPathVariableResolver(new VariableResolver(context));
        factory.setXPathFunctionResolver(new FunctionResolver(context, namespaceContext));
        XPath xpath = factory.newXPath();
        xpath.setNamespaceContext(this.namespaceContext);
        return xpath;
    }

}
