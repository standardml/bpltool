package com.beepell.repository;

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;

/**
 * Property Alias for WSDL Messages
 * @author Tim Hallwyl
 *
 */
public class MessagePropertyAlias extends PropertyAlias {

    private QName messageType;
    private String part;

    /**
     * Create MessagePropertyAlias object.
     * @param property
     * @param messageType
     * @param part
     * @param query 
     * @param namespaceContext 
     */
    public MessagePropertyAlias(Property property, QName messageType, String part, String query, NamespaceContext namespaceContext) {
        super(property, query, namespaceContext);
        this.messageType = messageType;
        this.part = part;
    }

    /**
     * Gets the message type QName.
     * @return the message type QName.
     */
    public QName getMessageType() {
        return messageType;
    }

    /**
     * Gets the part name.
     * @return the part name.
     */
    public String getPart() {
        return part;
    }
    
}
