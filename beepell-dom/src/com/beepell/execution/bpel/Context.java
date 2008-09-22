package com.beepell.execution.bpel;

import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.beepell.xml.namespace.NodeNamespaceContext;

/**
 * A class to get an set instance state values in the context of a node.
 * <p>
 * The context node is typically a WS-BPEL activity Element node, but it cloud
 * be any node within the instance tree; for example the from-node of a
 * copy-element within an Assign-activity.
 * 
 * @author Tim Hallwyl
 * 
 */
public class Context {

    /**
     * The context node.
     */
    private final Node node;

    private final XPath xPath;

    private final NamespaceContext namespaceContext;

    /**
     * Create a context for the specified node.
     * 
     * @param node The context node (must not be null).
     */
    public Context(final Node node) {
        if (node == null)
            throw new IllegalArgumentException("Context constructor does not accept a null Node.");
        this.node = node;

        this.namespaceContext = new NodeNamespaceContext(node);

        XPathFactory factory = XPathFactory.newInstance();
        this.xPath = factory.newXPath();
        this.xPath.setNamespaceContext(this.namespaceContext);

    }

    private Element getLinkElement(final String linkName) {

        try {
            final String expression = "(ancestor::bpi:flow/bpi:links/bpi:link[@name='" + linkName + "'])[last()]";
            Element link = (Element) this.xPath.evaluate(expression, this.node, XPathConstants.NODE);
            if (link == null)
                throw new IllegalStateException("Link '" + linkName + "' is not declared in the ancestor path.");

            return link;

        } catch (XPathExpressionException exception) {
            // TODO: Cause a WS-BPEL sub language fault
            exception.printStackTrace();
            return null;
        }

    }

    /**
     * Gets the state of the link.
     * 
     * @param linkName
     * @return null if the link is not set yet, otherwise the link state.
     */
    public Boolean getLinkState(final String linkName) {

        Element linkElement = getLinkElement(linkName);
        String state = linkElement.getAttribute("state");
        if (state.isEmpty())
            return null;

        return Boolean.valueOf(state);
    }

    /**
     * Sets the state of the link.
     * 
     * @param linkName The name of the link to be determined.
     * @param value The determined state value to set.
     */
    public void setLinkState(final String linkName, final boolean value) {

        Element linkElement = getLinkElement(linkName);
        if (!linkElement.getAttribute("state").isEmpty())
            throw new IllegalStateException("Link state for '" + linkName + "' has already been already determined.");

        linkElement.setAttribute("state", Boolean.toString(value));

        return;
    }


    
    
}
