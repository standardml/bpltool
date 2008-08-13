package com.beepell.repository;

import javax.xml.namespace.NamespaceContext;


/**
 * WS-BPEL Property Alias.
 * @author Tim Hallwyl
 *
 */
public abstract class PropertyAlias {

    protected final Property property;   
    protected final String query;
    protected final NamespaceContext namespaceContext;
    
    protected PropertyAlias(Property property, String query, NamespaceContext namespaceContext) {
        if (property == null)
            throw new IllegalArgumentException("ProertyAlias MUST have a Property assigned.");
        
        if (query != null && namespaceContext == null)
            throw new IllegalArgumentException("When a query is assign an alias, then namespace context MUST NOT be null.");
            
        this.property = property;
        this.query = query;
        this.namespaceContext = namespaceContext;
    }
    
    /**
     * Gets the Property this is alias for.
     * @return the Property this is alias for.
     */
    public Property getProperty() {
        return property;
    }
    
    /**
     * Gets the query string, if any.
     * @return null if no query is set, otherwise the query string.
     */
    public String getQuery() {
        return query;
    }

    
    /**
     * @return the namespaceContext
     */
    public NamespaceContext getNamespaceContext() {
        return namespaceContext;
    }
    
}
