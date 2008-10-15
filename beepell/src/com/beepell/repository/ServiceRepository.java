package com.beepell.repository;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.UnknownExtensibilityElement;
import javax.wsdl.extensions.soap.SOAPAddress;
import javax.wsdl.extensions.soap.SOAPBinding;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.beepell.BPELConstants;
import com.beepell.Settings;
import com.beepell.util.XML;
import com.beepell.xml.namespace.NodeNamespaceContext;

/**
 * A repository for receiving and containing definitions written in the Web
 * Service Definition Language (WSDL). Besides the WSDL standard definitions,
 * such as port type, operation and message, it contains WS-BPEL specific
 * definitions: Property, Property Alias, Partner Link Type and Roles. The WSDL
 * standard definitions are parsed using an implementation of JSR 110: Java APIs
 * for WSDL. Property, Property Alias, Partner Link Type and Roles are
 * implemented separately. Note that, process definitions does not share
 * repositories. Different process definitions may use different versions of
 * definitions in the namespace.
 * 
 * @author Tim Hallwyl
 */
public class ServiceRepository {
    private static final Logger log = Settings.getLogger();

    private final Hashtable<String, Definition> definitions = new Hashtable<String, Definition>();

    private final Hashtable<QName, Message> messages = new Hashtable<QName, Message>();

    private final Hashtable<QName, PartnerLinkType> partnerLinkTypes = new Hashtable<QName, PartnerLinkType>();

    private final Hashtable<QName, Property> properties = new Hashtable<QName, Property>();

    private final ArrayList<MessagePropertyAlias> messageAliases = new ArrayList<MessagePropertyAlias>();

    private final ArrayList<TypePropertyAlias> typeAliases = new ArrayList<TypePropertyAlias>();

    private final ArrayList<ElementPropertyAlias> elementAliases = new ArrayList<ElementPropertyAlias>();

    private final Hashtable<QName, Port> ports = new Hashtable<QName, Port>();

    /**
     * Adds a WSDL document to the repository.
     * 
     * @param uri location of the WSDL file.
     * @throws WSDLException
     */
    public void add(URI uri) throws WSDLException {
        WSDLFactory wsdlfactory = WSDLFactory.newInstance();
        WSDLReader reader = wsdlfactory.newWSDLReader();
        Definition definition = reader.readWSDL(uri.toString());

        definitions.put(definition.getTargetNamespace(), definition);
        addPartnerLinkTypes(definition);
        addMessages(definition);
        addProperties(definition);
        addPropertyAliases(definition);
        addPorts(definition);
    }

    @SuppressWarnings("unchecked")
    private void addPorts(Definition definition) {
        Collection<Service> services = definition.getServices().values();
        for (Service service : services) {
            Collection<Port> ports = service.getPorts().values();
            for (Port port : ports) {
                this.ports.put(port.getBinding().getQName(), port);
            }
        }
    }

    private void addMessages(Definition definition) {
        Iterator iterator = definition.getMessages().values().iterator();

        Message message;
        while (iterator.hasNext()) {
            message = (Message) iterator.next();
            this.messages.put(message.getQName(), message);
        }
    }

    private void addPartnerLinkTypes(Definition definition) {
        List list = definition.getExtensibilityElements();

        String targetNamespace = definition.getTargetNamespace();
        QName partnerLinkTypeName;
        PartnerLinkType partnerLinkType;
        Element element;
        String name, portType;
        Role role1, role2;

        UnknownExtensibilityElement uee;
        String info;
        for (Object object : list) {
            if (object instanceof UnknownExtensibilityElement) {
                uee = (UnknownExtensibilityElement) object;

                if (uee.getElementType().equals(new QName(BPELConstants.PLNK, "partnerLinkType"))) {
                    info = ("" + uee.getElementType().getLocalPart() + " '" + uee.getElement().getAttributes().getNamedItem("name").getNodeValue() + "':");
                    partnerLinkTypeName = new QName(targetNamespace, uee.getElement().getAttribute("name"));

                    NodeList roles = uee.getElement().getElementsByTagNameNS(BPELConstants.PLNK, "role");
                    info += ("      - Found " + roles.getLength() + " role(s): ");

                    element = (Element) roles.item(0);
                    name = element.getAttribute("name");
                    portType = element.getAttribute("portType");
                    role1 = new Role(name, definition.getPortType(XML.qualify(element, portType)));
                    info += (name + " -> " + portType);

                    if (roles.getLength() == 1) {
                        partnerLinkType = new PartnerLinkType(partnerLinkTypeName, role1);

                    } else {
                        element = (Element) roles.item(1);
                        name = element.getAttribute("name");
                        portType = element.getAttribute("portType");
                        role2 = new Role(name, definition.getPortType(XML.qualify(element, portType)));
                        partnerLinkType = new PartnerLinkType(partnerLinkTypeName, role1, role2);
                        info += (", " + name + " -> " + portType);
                    }

                    info += ("\n      - Saved as " + partnerLinkType.getName());
                    log.info(info);
                    this.partnerLinkTypes.put(partnerLinkType.getName(), partnerLinkType);

                }
            }
        }
    }

    private void addProperties(Definition definition) {
        List extensions = definition.getExtensibilityElements();
        Iterator iterator = extensions.iterator();

        while (iterator.hasNext()) {
            ExtensibilityElement extension = (ExtensibilityElement) iterator.next();

            if (extension.getElementType().equals(new QName(BPELConstants.VPROP, "property"))) {
                Element node = ((UnknownExtensibilityElement) extension).getElement();
                QName name = new QName(definition.getTargetNamespace(), node.getAttribute("name"));
                if (node.hasAttribute("type")) {
                    QName type = XML.qualify(node, node.getAttribute("type"));
                    properties.put(name, new TypeProperty(name, type));

                } else {
                    QName element = XML.qualify(node, node.getAttribute("element"));
                    properties.put(name, new ElementProperty(name, element));
                }
            }
        }
    }

    private void addPropertyAliases(Definition definition) {
        List extensions = definition.getExtensibilityElements();
        Iterator iterator = extensions.iterator();

        while (iterator.hasNext()) {
            ExtensibilityElement extension = (ExtensibilityElement) iterator.next();

            if (extension.getElementType().equals(new QName(BPELConstants.VPROP, "propertyAlias"))) {
                Element node = ((UnknownExtensibilityElement) extension).getElement();

                String query = null;
                Element queryNode = (Element) node.getElementsByTagNameNS(BPELConstants.VPROP, "query").item(0);
                if (queryNode != null) {
                    query = queryNode.getTextContent().trim();
                    if (query.isEmpty())
                        query = null;
                }
                QName propertyQName = XML.qualify(node, node.getAttribute("propertyName"));
                Property property = properties.get(propertyQName);
                NamespaceContext namespaceContext = new NodeNamespaceContext(queryNode == null ? node : queryNode);

                if (node.hasAttribute("messageType")) {
                    QName messageType = XML.qualify(node, node.getAttribute("messageType"));

                    String part = node.getAttribute("part");
                    if (part.isEmpty())
                        part = null;

                    messageAliases.add(new MessagePropertyAlias(property, messageType, part, query, namespaceContext));
                }
                if (node.hasAttribute("type")) {
                    QName type = XML.qualify(node, node.getAttribute("type"));
                    typeAliases.add(new TypePropertyAlias(property, type, query, namespaceContext));
                }
                if (node.hasAttribute("element")) {
                    QName element = XML.qualify(node, node.getAttribute("element"));
                    elementAliases.add(new ElementPropertyAlias(property, element, query, namespaceContext));
                }
            }
        }
    }

    /**
     * Returns the PartnerLinkType to which the specified QName is mapped, or
     * null if there is no mapping for the QName.
     * 
     * @param qName Qualified name of the PartnerLinkType to find.
     * @return the PartnerLinkType to which the specified QName is mapped, or
     *         null if there is no mapping for the QName.
     */
    public PartnerLinkType getPartnerLinkType(QName qName) {
        return partnerLinkTypes.get(qName);
    }

    /**
     * Returns the Message to which the specified QName is mapped, or null if
     * there is no mapping for the QName.
     * 
     * @param qName Qualified name of the Message to find.
     * @return the Message to which the specified QName is mapped, or null if
     *         there is no mapping for the QName.
     */
    public Message getMessage(QName qName) {
        return messages.get(qName);
    }

    /**
     * Returns a List of MessagePropertyAliases matching the QName given.
     * 
     * @param qName Qualified name of the Message type.
     * @return List of MessagePropertyAliases matching the QName given.
     */
    public List<MessagePropertyAlias> getPropertyAliasesByMessage(QName qName) {
        ArrayList<MessagePropertyAlias> list = new ArrayList<MessagePropertyAlias>();

        MessagePropertyAlias alias;
        for (int index = 0; index < messageAliases.size(); index++) {
            alias = messageAliases.get(index);
            if (alias.getMessageType().equals(qName))
                list.add(alias);
        }

        return list;
    }

    /**
     * Gets a WSDL binding, matching the port type, with a SOAP binding. If
     * there is more than one matching binding it is undefined which is
     * selected.
     * 
     * @param portType
     * @return null if not binding was found, otherwise the WSDL binding.
     */
    public Binding getBinding(QName portType) {

        Definition definition = definitions.get(portType.getNamespaceURI());
        if (definition == null)
            return null;
        
        Collection bindings = definition.getAllBindings().values();

        Iterator iterator = bindings.iterator();
        Binding binding;
        while (iterator.hasNext()) {
            binding = (Binding) iterator.next();
            if (binding.getPortType().getQName().equals(portType)) {
                log.info("Found a binding for portType '" + portType + "'.");
                // Found a binding matching the port type, check if it has a
                // SOAP binding.
                if (getExtension(binding.getExtensibilityElements(), SOAPBinding.class) != null)
                    return binding;
            }
        }
        return null;
    }

    /**
     * Gets the port for the binding. If more than one port is defined for the
     * binding, then it is undefined which is return by this method.
     * 
     * @param binding
     * @return the port for the binding.
     */
    public Port getPort(QName binding) {

        return ports.get(binding);

    }

    /**
     * Gets the first extension T in the list of extensions.
     * 
     * @param <T> Type extending WSDL ExtensibilityElement
     * @param list the list to look in
     * @param type the type to look for
     * @return the first extension T in the list of extensions.
     */
    @SuppressWarnings("unchecked")
    public static <T extends ExtensibilityElement> T getExtension(List list, Class<T> type) {
        Iterator iterator = list.iterator();
        ExtensibilityElement element;

        while (iterator.hasNext()) {
            element = (ExtensibilityElement) iterator.next();
            if (type.isInstance(element))
                return (T) element;
        }

        return null;
    }

    /**
     * Gets a list of MessagePropertyAlias for the property.
     * 
     * @param property
     * @return a list of MessagePropertyAlias for the property.
     */
    public List<MessagePropertyAlias> getMessagePropertyAliases(QName property) {
        ArrayList<MessagePropertyAlias> list = new ArrayList<MessagePropertyAlias>();

        for (MessagePropertyAlias alias : this.messageAliases) {
            if (alias.getProperty().getName().equals(property))
                list.add(alias);
        }

        return list;
    }

    /**
     * Gets a list of ElementPropertyAlias for the property.
     * 
     * @param property
     * @return a list of ElementPropertyAlias for the property.
     */
    public List<ElementPropertyAlias> getElementPropertyAliases(QName property) {
        ArrayList<ElementPropertyAlias> list = new ArrayList<ElementPropertyAlias>();

        for (ElementPropertyAlias alias : this.elementAliases) {
            if (alias.getProperty().getName().equals(property))
                list.add(alias);
        }

        return list;
    }

    /**
     * Gets the Property
     * 
     * @param propertyName
     * @return the named Property.
     */
    public Property getProperty(QName propertyName) {
        return properties.get(propertyName);
    }

    /**
     * Gets a list of TypePropertyAlias for the property.
     * 
     * @param propertyName
     * @return a list of TypePropertyAlias for the property.
     */
    public List<TypePropertyAlias> getTypePropertyAliases(QName propertyName) {
        ArrayList<TypePropertyAlias> list = new ArrayList<TypePropertyAlias>();

        for (TypePropertyAlias alias : this.typeAliases) {
            if (alias.getProperty().getName().equals(propertyName))
                list.add(alias);
        }

        return list;
    }

    /**
     * Gets the address at which myRole of partnerLinkName is exposed.
     * 
     * @param partnerLinkTypeName
     * @param myRole
     * @return the address at which myRole of partnerLinkName is exposed.
     */
    public String getMyAddress(QName partnerLinkTypeName, String myRole) {
        Port port = this.getPort(partnerLinkTypeName, myRole);
        String address = ((SOAPAddress) port.getExtensibilityElements().get(0)).getLocationURI();
        return address;
    }

    /**
     * Gets the port which role of partnerLinkName is exposed.
     * 
     * @param partnerLinkTypeName
     * @param role
     * @return the port which role of partnerLinkName is exposed.
     */
    public Port getPort(QName partnerLinkTypeName, String role) {
        PartnerLinkType partnerLinkType = this.getPartnerLinkType(partnerLinkTypeName);
        PortType portType = partnerLinkType.getPortType(role);

        Binding binding = this.getBinding(portType.getQName());
        if (binding == null)
            throw new IllegalStateException("No appropriate binding was found for port type '" + portType.getQName() + "'.");

        return this.getPort(binding.getQName());
    }

}
