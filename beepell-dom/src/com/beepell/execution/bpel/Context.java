package com.beepell.execution.bpel;

import java.util.List;

import javax.wsdl.Message;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.beepell.BPELConstants;
import com.beepell.exceptions.SubLanguageExecutionFault;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.xml.namespace.NodeNamespaceContext;
import com.beepell.xml.xpath.FunctionResolver;
import com.beepell.xml.xpath.LinkStateResolver;
import com.beepell.xml.xpath.VariableResolver;
import com.sun.xml.xsom.XSType;

/**
 * A class to get an set instance state values in the context of a node.
 * <p>
 * The context node is typically a WS-BPEL activity Element node, but it may be
 * any node within the instance tree; for example the from-node of a
 * copy-element within an Assign-activity.
 * <p>
 * However, some methods are only to be used in the context of an activity
 * Element node, such as isSyncronizing(), and will throw an unchecked
 * IllegalStateException if used with a node other that an activity.
 * 
 * @author Tim Hallwyl
 * 
 */
public class Context {

    /**
     * The repository with all XML Schema definitions.
     */
    private final SchemaRepository schemas;

    /**
     * The repository with all service definitions.
     */
    private final ServiceRepository services;

    /**
     * The context node.
     */
    private final Node node;

    /**
     * Factory to create XPath objects. Most methods use the default xPath
     * object below, but some needs special variable and/or function resolvers.
     */
    private final XPathFactory factory;

    /**
     * The default XPath object used by most methods.
     */
    private final XPath xPath;

    /**
     * The namespace context used by XPath to lookup a namespace from a prefix.
     */
    private final NamespaceContext namespaceContext;

    /**
     * Create a Context object for the specified node.
     * <p>
     * The schemas and services parameters may be null if the repository is
     * stored in the owner document's user data.
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

        this.factory = XPathFactory.newInstance();
        this.xPath = this.factory.newXPath();
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

    /**
     * Gets the Link declaration Element node.
     * 
     * @param linkName Name of the link to get.
     * @return The 'link' Element node, declared in an ancestor Flow activity.
     */
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
     * Gets the state of the link: null, true or false.
     * 
     * @param linkName The name of the link.
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
     * @throws IllegalStateException If the link has already been determined.
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

    /**
     * Extracts the value node from a variable Element node.
     * 
     * @param variableElement The variable Element node.
     * @return The value node of a variable or null if the variable has no
     *         value.
     */
    private Node getContainedValue(Element variableElement) {
        try {
            // TODO Throw WS-BPEL uninitialized variable if not initialized
            if (variableElement.getFirstChild() == null)
                return null;

            String type = variableElement.getAttribute("type");
            if (!type.isEmpty()) {
                // This is a simple or complex typed variable

                XSType xsType = this.schemas.getType(qualify(type));

                if (xsType == null)
                    throw new IllegalStateException("Type " + type + " is not declared.");

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
     * Gets the variable type.
     * 
     * @param name The variable name.
     * @return The qualified name of the type.
     */
    public QName getVariableType(String name) {
        Element variableElement = getVariableElement(name);
        String type = variableElement.getAttribute("type");
        if (type.isEmpty())
            type = variableElement.getAttribute("element");
        if (type.isEmpty())
            type = variableElement.getAttribute("messageType");
        return this.qualify(type);
    }

    /**
     * Gets the variable type.
     * 
     * @param name The message variable name.
     * @param part The message part name.
     * @return The qualified name of the type.
     */
    public QName getVariableType(String name, String part) {
        Element variableElement = getVariableElement(name);
        String type = variableElement.getAttribute("messageType");
        Message message = this.services.getMessage(this.qualify(type));
        return message.getPart(part).getTypeName();
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
        return getContainedValue(variableElement);
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
                    return getContainedValue((Element) node);
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

    /**
     * Gets the partner link Element node from an ancestor Scope node.
     * 
     * @param name The partner link name to lookup.
     * @return The partner link Element node.
     * @throws IllegalStateException If the partner link is not found in the
     *             context.
     */
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
        while (node != null) {
            if (node instanceof Element && endpoint.equals(node.getLocalName())) {
                return Utils.getFirstChildElement((Element) node);
            }
            node = node.getNextSibling();
        }

        return null;
    }

    /**
     * Get the named correlation set in context.
     * 
     * @param name The name of the correlation set to get.
     * @return The CorrelationSet Element node.
     */
    public Element getCorrelationSet(String name) {
        try {
            final String expression = "(ancestor::bpi:*/bpi:correlationSets/bpi:correlationSet[@name='" + name + "'])[last()]";
            Element partnerLinkElement = (Element) this.xPath.evaluate(expression, this.node, XPathConstants.NODE);

            if (partnerLinkElement == null)
                throw new IllegalStateException("Correlation set '" + name + "' is not declared in the ancestor path.");

            return partnerLinkElement;

        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            return null;
        }
    }

    /**
     * Get the initialized correlation value form the named correlation set in
     * context.
     * <p>
     * This method returns null if the correlation value has not been
     * initialized.
     * 
     * @param set The name of the correlation set.
     * @param property The qualified name of the property.
     * @return The correlation property value or null if it is not initialized.
     */
    public Node getCorrelationValue(String set, QName property) {
        if (property == null)
            throw new IllegalArgumentException("A null value is not allowed for parameter 'property'");

        Element correlationSetElement = this.getCorrelationSet(set);

        Node child = correlationSetElement.getFirstChild();
        while (child != null) {

            if ("property".equals(child.getLocalName())) {
                QName propertyName = this.qualify(((Element) child).getAttribute("name"));
                if (property.equals(propertyName)) {
                    return this.getContainedValue((Element) child);
                }
            }

            child = child.getNextSibling();
        }

        return null;
    }

    /**
     * Evaluates the join condition.
     * <p>
     * If the evaluation fails of some reason, this method returns false. If the
     * element does not have any targets this methods returns true.
     * <p>
     * Note: This method returns true if there is no incoming links (targets).
     * 
     * @return true if the join condition evaluated true, otherwise false.
     * @throws IllegalStateException If the context node is not an activity
     *             element;
     */
    public boolean evaluateJoinCondition() {

        if (!(this.node instanceof Element) || !Utils.isActivity((Element) this.node))
            throw new IllegalStateException("Cannot evaluate the join condition: The context node is not an activity.");

        if (!Utils.hasTargets((Element) this.node))
            return true;

        XPath xPath = this.factory.newXPath();
        xPath.setNamespaceContext(this.namespaceContext);
        xPath.setXPathVariableResolver(new LinkStateResolver(this));
        try {
            Boolean result = (Boolean) xPath.evaluate(Utils.getJoinCondition((Element) this.node), this.node, XPathConstants.BOOLEAN);
            return result.booleanValue();
        } catch (XPathExpressionException exception) {
            return false;
        }
    }

    /**
     * Checks if the activity is synchronizing; that is, if it is waiting for
     * some incoming link to be determined.
     * <p>
     * Note: If this returns true, then the next step is to evaluate the join
     * condition.
     * 
     * @return true if the activity is waiting for one or incoming links, false
     *         if all incoming links are determined.
     */
    public boolean isSynchronizing() {
        if (!(this.node instanceof Element) || !Utils.isActivity((Element) this.node))
            throw new IllegalStateException("Cannot check links: The context node is not an activity.");

        Element element = (Element) this.node;
        if (!Utils.hasTargets(element))
            return false;

        List<String> targets = Utils.getTargetsNames(element);
        if (targets == null)
            return false;

        Context context = new Context(element);
        for (String linkName : targets) {
            if (context.getLinkState(linkName) == null)
                return true;
        }

        return false;

    }

    /**
     * Removes all incoming links from the context node (provided that is is an
     * activity Element) and from the Flow activities where they are declared.
     * <p>
     * Note: This method does not check if the links are determined. This method
     * is to be called only when the activity is no longer synchronizing and the
     * join condition has been evaluated (disregarding the outcome).
     */
    public void removeIncomingLinks() {
        if (!(this.node instanceof Element) || !Utils.isActivity((Element) this.node))
            throw new IllegalStateException("Cannot remove incoming links: The context node is not an activity.");

        List<String> linkNames = Utils.getTargetsNames((Element) this.node);
        if (linkNames == null)
            return;

        Element link;
        for (String linkName : linkNames) {
            link = this.getLinkElement(linkName);
            Utils.remove(link);
        }

        try {

            Element targets = (Element) this.xPath.evaluate("bpi:targets", this.node, XPathConstants.NODE);
            Utils.remove(targets);

        } catch (XPathExpressionException exception) {
            /*
             * Should not happen since linkNames != null there must be a targets
             * element
             */
            exception.printStackTrace();
            throw new IllegalStateException("Failed to remove targets element.");
        }
    }

    /**
     * Move the outgoing links from the context node (provided that is is an
     * activity Element node) to the 'to' node.
     * <p>
     * The links moved will be added to the end of the end of the 'to' node's
     * sources. Each link Element moved will have a'inherit' attribute added.
     * <p>
     * If the 'to' Element node does not have a 'sources' element, one is
     * created.
     * 
     * @param to The activity Element node that will inherit the context node's
     *            outgoing links.
     * @throws IllegalStateException If the context node or the 'to' Element are
     *             not an activity Elements or if the method fails.
     */
    public void inheritOutgoingLinks(Element to) {
        if (!(this.node instanceof Element) || !Utils.isActivity((Element) this.node))
            throw new IllegalStateException("Cannot remove incoming links: The context node is not an activity.");

        if (!Utils.isActivity(to))
            throw new IllegalStateException("Cannot remove incoming links: The 'to' node is not an activity.");

        try {
            Element childSourceList = (Element) this.xPath.evaluate("bpi:sources", to, XPathConstants.NODE);
            if (childSourceList == null) {
                childSourceList = (Element) to.appendChild(to.getOwnerDocument().createElementNS(BPELConstants.BPI, "sources"));
            }

            NodeList sources = (NodeList) this.xPath.evaluate("bpi:sources/bpi:source", this.node, XPathConstants.NODESET);
            Node parentNode = sources.item(0).getParentNode();

            Element source;
            for (int i = 0; i < sources.getLength(); i++) {
                source = (Element) sources.item(i);
                source = (Element) parentNode.removeChild(source);
                childSourceList.appendChild(source);
            }

            parentNode.getParentNode().removeChild(parentNode);

        } catch (XPathExpressionException exception) {
            throw new IllegalStateException("Failed to inherit links from " + this.node.getLocalName() + " to " + to.getLocalName(), exception);
        }

    }

    /**
     * @param expression The boolean expression to evaulate.
     * @return The boolean result of the evaluation.
     * @throws SubLanguageExecutionFault
     */
    public boolean evaluateBoolean(final String expression) throws SubLanguageExecutionFault {
        try {
            this.xPath.setXPathFunctionResolver(new FunctionResolver(this, this.namespaceContext));
            this.xPath.setXPathVariableResolver(new VariableResolver(this));
            Boolean result = (Boolean) this.xPath.evaluate(expression, this.node, XPathConstants.BOOLEAN);
            return result.booleanValue();
        } catch (Exception exception) {
            throw new SubLanguageExecutionFault(exception);
        }
    }

    /**
     * @return the schemas
     */
    public SchemaRepository getSchemas() {
        return this.schemas;
    }

    /**
     * @return the services
     */
    public ServiceRepository getServices() {
        return this.services;
    }

    /**
     * Gets the variable property value.
     * 
     * @param variable The variable name.
     * @param property The qualified name of the variable property.
     * @return The variable property value.
     */
    public Node getVariablePropertyValue(String variable, QName property) {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * Gets the qualified name of the schema type of the variable property.
     * 
     * @param property qualified name of the variable property
     * @return the variable property type
     */
    public QName getVariablePropertyType(QName property) {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * Returns true if the source and target of the link are in different
     * scopes.
     * 
     * @param linkName The name of the link.
     * @return true if the link is inter-scope, otherwise false.
     */
    public boolean isInterScope(String linkName) {
        // TODO Auto-generated method stub
        return false;
    }

}
