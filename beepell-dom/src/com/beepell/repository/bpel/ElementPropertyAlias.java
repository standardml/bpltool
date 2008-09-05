package com.beepell.repository.bpel;

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;


/**
 * ElementPropertyAlias class.
 * <p>
 * This is a WS-BPEL 2.0 specific extension.
 * 
 * @author Tim Hallwyl
 * 
 */
public class ElementPropertyAlias extends PropertyAlias {

    private QName element;

    /**
     * Creates an Element Property Alias.
     * 
     * @param property
     * @param element
     * @param query
     * @param namespaceContext
     */
    public ElementPropertyAlias(Property property, QName element, String query, NamespaceContext namespaceContext) {
        super(property, query, namespaceContext);
        this.element = element;
    }

    /**
     * Gets the element QName.
     * 
     * @return the element QName.
     */
    public QName getElement() {
        return this.element;
    }

}
