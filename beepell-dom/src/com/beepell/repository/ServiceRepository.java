package com.beepell.repository;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
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
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.beepell.BPELConstants;
import com.beepell.repository.bpel.ElementProperty;
import com.beepell.repository.bpel.ElementPropertyAlias;
import com.beepell.repository.bpel.MessagePropertyAlias;
import com.beepell.repository.bpel.PartnerLinkType;
import com.beepell.repository.bpel.Property;
import com.beepell.repository.bpel.Role;
import com.beepell.repository.bpel.TypeProperty;
import com.beepell.repository.bpel.TypePropertyAlias;
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

        this.definitions.put(definition.getTargetNamespace(), definition);
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

    @SuppressWarnings("unchecked")
    private void addMessages(Definition definition) {
        Iterator<Message> iterator = definition.getMessages().values().iterator();

        Message message;
        while (iterator.hasNext()) {
            message = iterator.next();
            this.messages.put(message.getQName(), message);
        }
    }

    @SuppressWarnings("unchecked")
    private void addPartnerLinkTypes(Definition definition) {
        List<Object> list = definition.getExtensibilityElements();

        String targetNamespace = definition.getTargetNamespace();
        QName partnerLinkTypeName;
        PartnerLinkType partnerLinkType;
        Element element;
        String name, portType;
        Role role1, role2;

        UnknownExtensibilityElement uee;
        for (Object object : list) {
            if (object instanceof UnknownExtensibilityElement) {
                uee = (UnknownExtensibilityElement) object;

                if (uee.getElementType().equals(new QName(BPELConstants.PLNK, "partnerLinkType"))) {
                    System.out.println("INFO: " + uee.getElementType().getLocalPart() + " '"
                            + uee.getElement().getAttributes().getNamedItem("name").getNodeValue() + "':");
                    partnerLinkTypeName = new QName(targetNamespace, uee.getElement().getAttribute("name"));

                    NodeList roles = uee.getElement().getElementsByTagNameNS(BPELConstants.PLNK, "role");
                    System.out.print("      - Found " + roles.getLength() + " role(s): ");

                    element = (Element) roles.item(0);
                    name = element.getAttribute("name");
                    portType = element.getAttribute("portType");
                    role1 = new Role(name, definition.getPortType(qualify(element, portType)));
                    System.out.print(name + " -> " + portType);

                    if (roles.getLength() == 1) {
                        partnerLinkType = new PartnerLinkType(partnerLinkTypeName, role1);

                    } else {
                        element = (Element) roles.item(1);
                        name = element.getAttribute("name");
                        portType = element.getAttribute("portType");
                        role2 = new Role(name, definition.getPortType(qualify(element, portType)));
                        partnerLinkType = new PartnerLinkType(partnerLinkTypeName, role1, role2);
                        System.out.print(", " + name + " -> " + portType);
                    }

                    System.out.println("\n      - Saved as " + partnerLinkType.getName());
                    this.partnerLinkTypes.put(partnerLinkType.getName(), partnerLinkType);

                }
            }
        }
    }

    @SuppressWarnings("unchecked")
    private void addProperties(Definition definition) {
        List<ExtensibilityElement> extensions = definition.getExtensibilityElements();
        Iterator<ExtensibilityElement> iterator = extensions.iterator();

        while (iterator.hasNext()) {
            ExtensibilityElement extension = iterator.next();

            if (extension.getElementType().equals(new QName(BPELConstants.VPROP, "property"))) {
                Element node = ((UnknownExtensibilityElement) extension).getElement();
                QName name = new QName(definition.getTargetNamespace(), node.getAttribute("name"));
                if (node.hasAttribute("type")) {
                    QName type = qualify(node, node.getAttribute("type"));
                    this.properties.put(name, new TypeProperty(name, type));

                } else {
                    QName element = qualify(node, node.getAttribute("element"));
                    this.properties.put(name, new ElementProperty(name, element));
                }
            }
        }
    }

    @SuppressWarnings("unchecked")
    private void addPropertyAliases(Definition definition) {
        List<ExtensibilityElement> extensions = definition.getExtensibilityElements();
        Iterator<ExtensibilityElement> iterator = extensions.iterator();

        while (iterator.hasNext()) {
            ExtensibilityElement extension = iterator.next();

            if (extension.getElementType().equals(new QName(BPELConstants.VPROP, "propertyAlias"))) {
                Element node = ((UnknownExtensibilityElement) extension).getElement();

                String query = null;
                Element queryNode = (Element) node.getElementsByTagNameNS(BPELConstants.VPROP, "query").item(0);
                if (queryNode != null) {
                    query = queryNode.getTextContent().trim();
                    if (query.isEmpty())
                        query = null;
                }
                QName propertyQName = qualify(node, node.getAttribute("propertyName"));
                Property property = this.properties.get(propertyQName);
                NamespaceContext namespaceContext = new NodeNamespaceContext(queryNode == null ? node : queryNode);

                if (node.hasAttribute("messageType")) {
                    QName messageType = qualify(node, node.getAttribute("messageType"));

                    String part = node.getAttribute("part");
                    if (part.isEmpty())
                        part = null;

                    this.messageAliases.add(new MessagePropertyAlias(property, messageType, part, query, namespaceContext));
                }
                if (node.hasAttribute("type")) {
                    QName type = qualify(node, node.getAttribute("type"));
                    this.typeAliases.add(new TypePropertyAlias(property, type, query, namespaceContext));
                }
                if (node.hasAttribute("element")) {
                    QName element = qualify(node, node.getAttribute("element"));
                    this.elementAliases.add(new ElementPropertyAlias(property, element, query, namespaceContext));
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
        return this.partnerLinkTypes.get(qName);
    }

    /**
     * Gets the WSDL Message definition object.
     * 
     * @param messageName The qualified name of the Message to find.
     * @return The WSDL Message definition, or null if
     *         there is no message by the specified QName.
     */
    public Message getMessage(QName messageName) {
        return this.messages.get(messageName);
    }

    /**
     * Returns a List of MessagePropertyAliases defined for the specified message.
     * 
     * @param messageName The qualified name of the Message.
     * @return List of MessagePropertyAliases.
     */
    public List<MessagePropertyAlias> getPropertyAliasesByMessage(QName messageName) {
        ArrayList<MessagePropertyAlias> list = new ArrayList<MessagePropertyAlias>();

        MessagePropertyAlias alias;
        for (int index = 0; index < this.messageAliases.size(); index++) {
            alias = this.messageAliases.get(index);
            if (alias.getMessageType().equals(messageName))
                list.add(alias);
        }

        return list;
    }

    /**
     * Gets the binding operation definition.
     * 
     * @param binding The qualified name of the binding.
     * @param operation The name of the operation.
     * @return The BindingOperation for the specified operation and binding.
     */
    public BindingOperation getBindingOperation(QName binding, String operation) {

        Definition definition = this.definitions.get(binding.getNamespaceURI());
        return definition.getBinding(binding).getBindingOperation(operation, null, null);

    }

    /**
     * Gets the binding definition by its qualified name.
     * 
     * @param binding The qualified name of the binding.
     * @return The binding definition.
     */
    public Binding getBinding(QName binding) {
        Definition definition = this.definitions.get(binding.getNamespaceURI());
        return definition.getBinding(binding);
    }

    /**
     * Gets a WSDL binding, matching the port type, with a SOAP binding. If
     * there is more than one matching binding it is undefined which is
     * selected.
     * 
     * @param portType
     * @return null if not binding was found, otherwise the WSDL binding.
     */
    @SuppressWarnings({ "unchecked", "cast" })
    public Binding getMatchingBinding(QName portType) {

        Definition definition = this.definitions.get(portType.getNamespaceURI());
        if (definition == null)
            return null;

        Collection<Binding> bindings = (Collection<Binding>) definition.getAllBindings().values();

        Iterator<Binding> iterator = bindings.iterator();
        Binding binding;
        while (iterator.hasNext()) {
            binding = iterator.next();
            if (binding.getPortType().getQName().equals(portType)) {
                System.out.println("INFO: Found a binding for portType '" + portType + "'.");
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
    public Port getMatchingPort(QName binding) {

        return this.ports.get(binding);

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
        return this.properties.get(propertyName);
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

        Binding binding = this.getMatchingBinding(portType.getQName());
        if (binding == null)
            throw new IllegalStateException("No appropriate binding was found for port type '" + portType.getQName() + "'.");

        return this.getMatchingPort(binding.getQName());
    }

    /**
     * Qualify a prefixed name with it's namespace URI. If the qname string does
     * not contain a prefix, the default namespace is used.
     * 
     * @param node The context to lookup namespace URI matching the prefix.
     * @param qname The prefixed QName string.
     * @return A QName with local name, namespace URI and prefix.
     */
    public static QName qualify(Node node, String qname) {
        if (node == null)
            throw new IllegalArgumentException("Node must not be null.");

        if (qname == null)
            throw new IllegalArgumentException("QName must not be null.");

        if (qname.isEmpty())
            throw new IllegalArgumentException("QName must not be an empty string.");

        String[] split = qname.split(":");
        String uri;

        if (split.length == 1) {
            uri = node.lookupNamespaceURI(null);
            return new QName(uri, split[1]);
        }
    
        uri = node.lookupNamespaceURI(split[0]);
        return new QName(uri, split[1], split[0]);

    }
    
    /**
     * Gets the port type definition by its qualified name.
     * @param portTypeName The qualified name of the port type.
     * @return The WSDL port type definition, or null if the port type was not found.
     */
    public PortType getPortType(QName portTypeName) {
        
        Definition definition = this.definitions.get(portTypeName.getNamespaceURI());
        if (definition == null)
            return null;
        
        return definition.getPortType(portTypeName);
    }
}
