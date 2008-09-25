package com.beepell.execution.bpel;

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.xml.namespace.NodeNamespaceContext;
import com.sun.xml.xsom.XSType;

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

    private final SchemaRepository schemas;
    private final ServiceRepository services;

    /**
     * The context node.
     */
    private final Node node;

    private final XPath xPath;

    private final NamespaceContext namespaceContext;

    /**
     * Create a context for the specified node.
     * <p>
     * The schemas and services parameters may be null if the repository is
     * stored in the owner documents user data-.
     * 
     * @param node The context node (must not be null).
     * @param schemas Repository of imported XML Schema definitions.
     * @param services Repository of imported WSDL definitions.
     */
    public Context(final Node node, final SchemaRepository schemas, final ServiceRepository services) {
        if (node == null)
            throw new IllegalArgumentException("Context constructor does not accept a null Node.");
        this.node = node;

        this.namespaceContext = new NodeNamespaceContext(node);

        XPathFactory factory = XPathFactory.newInstance();
        this.xPath = factory.newXPath();
        this.xPath.setNamespaceContext(this.namespaceContext);

        if (schemas == null) {
            this.schemas = (SchemaRepository) node.getOwnerDocument().getUserData("com.beepell.repository.SchemaRepository");
            if (this.schemas == null)
                throw new IllegalArgumentException("A SchemaRepository was not found in the Document.");
        } else {
            this.schemas = schemas;
        }

        if (services == null) {
            this.services = (ServiceRepository) node.getOwnerDocument().getUserData("com.beepell.repository.ServiceRepository");
            if (this.services == null)
                throw new IllegalArgumentException("A ServiceRepository was not found in the Document.");
        } else {
            this.services = services;
        }

    }

    /**
     * Create a context for the specified node.
     * <p>
     * Service and Schema repositories are read from the owner document´s user
     * data. This is the same as Context(node, null, null).
     * 
     * @param node The context node (must not be null).
     */
    public Context(final Node node) {
        this(node, null, null);
    }

    private Element getLinkElement(final String linkName) {

        try {
            final String expression = "(ancestor::bpi:flow/bpi:links/bpi:link[@name='" + linkName + "'])[last()]";
            Element linkElement = (Element) this.xPath.evaluate(expression, this.node, XPathConstants.NODE);
            if (linkElement == null)
                throw new IllegalStateException("Link '" + linkName + "' is not declared in the ancestor path.");

            return linkElement;

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

    /**
     * Gets the Element node declaring the named variable in the context.
     * 
     * @param name The name of the variable to get.
     * @return The Element node declaring the variable.
     * @throws IlligalStateException if the variable name is not declared.
     */
    private Element getVariableElement(String name) {
        try {
            final String expression = "(ancestor::bpi:*/bpi:variables/bpi:variable[@name='" + name + "'])[last()]";
            Element variableElement = (Element) this.xPath.evaluate(expression, this.node, XPathConstants.NODE);
            if (variableElement == null)
                throw new IllegalStateException("Variable '" + name + "' is not declared in the ancestor path.");

            return variableElement;

        } catch (XPathExpressionException exception) {
            // TODO: Cause a WS-BPEL sub language fault
            exception.printStackTrace();
            return null;
        }
    }

    private Node getVariableValue(Element variableElement) {
        try {
            // TODO Throw WS-BPEL uninitialized variable if not initialized
            if (variableElement.getFirstChild() == null)
                return null;

            String type = variableElement.getAttribute("type");
            if (!type.isEmpty()) {
                // This is a simple or complex typed variable

                XSType xsType = this.schemas.getType(qualify(type));

                if (xsType.isSimpleType()) {
                    // TODO Normalize document to ensure that there is only one
                    // text node.
                    return variableElement.getFirstChild();
                }

                // Complex Type
                return Utils.getFirstChildElement(variableElement);

            }

            String element = variableElement.getAttribute("element");
            if (!element.isEmpty()) {
                // This is an element variable.
                // Value is the element contained by the variable element.
                return Utils.getFirstChildElement(variableElement);
            }

            String messageType = variableElement.getAttribute("messageType");
            if (!messageType.isEmpty()) {
                // This is a message type variable
                return null;
            }

            return null;

        } catch (SAXException exception) {
            // The variable type was not found using getType
            // TODO: Handle fault show how
            throw new IllegalStateException(exception);
        }

    }

    /**
     * Gets the variable value.
     * <p>
     * Note: This method will return null when access to a message type variable
     * is attempted. For message typed variables, access is only per part - see
     * getVariableValue(String name, String part).
     * 
     * @param name The name of the variable.
     * @return The variable value, as a node.
     */
    public Node getVariableValue(String name) {
        Element variableElement = getVariableElement(name);
        return getVariableValue(variableElement);
    }

    /**
     * Gets the part value of a message typed variable.
     * 
     * @param name The message variable name.
     * @param part The name of the part to get.
     * @return The message variable part value.
     */
    public Node getVariableValue(String name, String part) {
        Element variableElement = getVariableElement(name);

        // TODO Throw WS-BPEL uninitialized variable if not initialized
        if (variableElement.getFirstChild() == null)
            return null;

        String messageType = variableElement.getAttribute("messageType");
        if (!messageType.isEmpty()) {
            // This is indeed a message type variable

            Node node = variableElement.getFirstChild();
            while (node != null) {
                if (node instanceof Element && node.getLocalName().equals(part))
                    return getVariableValue((Element) node);
                node = node.getNextSibling();
            }
            return null;

        }

        // This is not a message variable!
        return null;
    }

    /**
     * Qualify a prefixed name with its namespace URI. If the qname string does
     * not contain a prefix, the default namespace is used.
     * 
     * @param qname The prefixed QName string.
     * @return A QName with local name, namespace URI and prefix.
     */
    public QName qualify(String qname) {
        if (this.node == null)
            throw new IllegalArgumentException("Node must not be null.");

        if (qname == null)
            throw new IllegalArgumentException("QName must not be null.");

        if (qname.isEmpty())
            throw new IllegalArgumentException("QName must not be an empty string.");

        String[] split = qname.split(":");
        String uri;

        if (split.length == 1) {
            uri = this.node.lookupNamespaceURI(null);
            return new QName(uri, split[1]);
        }

        uri = this.node.lookupNamespaceURI(split[0]);
        return new QName(uri, split[1], split[0]);

    }
    private Element getPartnerLinkElement(String name) {
        try {
            final String expression = "(ancestor::bpi:*/bpi:partnerLinks/bpi:partnerLink[@name='" + name + "'])[last()]";
            Element partnerLinkElement = (Element) this.xPath.evaluate(expression, this.node, XPathConstants.NODE);
            if (partnerLinkElement == null)
                throw new IllegalStateException("Partner Link '" + name + "' is not declared in the ancestor path.");

            return partnerLinkElement;

        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            return null;
        }
        
    }

    /**
     * Gets the endpoint (wsdl:port) Element for the specified partner link and
     * role.
     * 
     * @param partnerLinkName
     * @param role myRole or parnterRole
     * @return The WSDL Port Element or null if the endpoint has not been
     *         initialized.
     */
    public Element getEndpoint(final String partnerLinkName, final Role role) {
        Element partnerLinkElement = getPartnerLinkElement(partnerLinkName);
        String endpoint;
        if (role.equals(Role.MY))
            endpoint = "myRoleEndpoint";
        else
            endpoint = "partnerRoleEndpoint";
        
        Node node = partnerLinkElement.getFirstChild();
        while(node != null) {
            if (node instanceof Element && endpoint.equals(node.getLocalName())) {
                return Utils.getFirstChildElement((Element) node);
            }
            node = node.getNextSibling();
        }
        
        return null;
    }
}
