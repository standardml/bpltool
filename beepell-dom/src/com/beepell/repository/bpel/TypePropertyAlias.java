package com.beepell.repository.bpel;

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;


/**
 * Property Alias for XML Schema types.
 * 
 * <p>
 * This is a WS-BPEL 2.0 specific extension.
 * 
 * @author Tim Hallwyl
 * 
 */
public class TypePropertyAlias extends PropertyAlias {

    private QName type;

    /**
     * Creates PropertyAlias for schema types.
     * 
     * @param property
     * @param type
     * @param query
     * @param namespaceContext
     */
    public TypePropertyAlias(Property property, QName type, String query, NamespaceContext namespaceContext) {
        super(property, query, namespaceContext);
        this.type = type;
    }

    /**
     * Gets the schema type QName.
     * 
     * @return the schema type QName.
     */
    public QName getType() {
        return this.type;
    }

}
