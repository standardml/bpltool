package com.beepell.broker;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.w3c.dom.Node;

/**
 * This is a representation of the abstract WSDL 1.1 message. This is
 * independent of the specific protocol used to send and receive messages.
 * 
 * @author Tim Hallwyl
 * 
 */
public class Message {

    /**
     * This it the qualified name of the message.
     */
    private final QName messageName;

    /**
     * This is a map of part names and their content.
     */
    private final Map<String, Node> parts;

    /**
     * The qualified name of the port type.
     */
    private final QName portTypeName;

    /**
     * This is the (unqualified) name of the operation. This must be a valid
     * operation name of the port type.
     */
    private final String operationName;

    /**
     * The binding used at the Port (end-point).
     */
    private final QName bindingName;

    /**
     * The address the message is send to.
     */
    private final URI endpoint;

    /**
     * Create a Message.
     * @param type The WSDL message definition.
     * @param portType The WSDL port type.
     * @param operation The WSDL operation.
     * @param binding The WSDL binding.
     * @param endpoint The protocol specific end-point address.
     * 
     */
    public Message(QName type, QName portType, String operation, QName binding, URI endpoint) {
        this.messageName = type;
        this.portTypeName = portType;
        this.operationName = operation;
        this.bindingName = binding;
        this.endpoint = endpoint;
        this.parts = new HashMap<String, Node>();
    }

    /**
     * Gets the target operation name.
     * 
     * @return Qualified name of the target operation.
     */
    public String getOperationName() {
        return this.operationName;
    }

    /**
     * Gets the qualified name of the port type (interface) that this message
     * was received on or is to be send to.
     * 
     * @return Qualified name of the port type.
     */
    public QName getPortTypeName() {
        return this.portTypeName;
    }

    /**
     * Gets the qualified name of the message definition.
     * 
     * @return the qualified name of the message definition.
     */
    public QName getDefinitionName() {
        return this.messageName;
    }

    /**
     * Get the message parts in a Map from part name (String) to value (Node).
     * 
     * @return The message parts.
     */
    public Map<String, Node> getParts() {
        return this.parts;
    }

    /**
     * @return the messageName
     */
    public QName getMessageName() {
        return this.messageName;
    }

    /**
     * @return the bindingName
     */
    public QName getBindingName() {
        return this.bindingName;
    }

    /**
     * @return the endpoint
     */
    public URI getEndpoint() {
        return this.endpoint;
    }

}
