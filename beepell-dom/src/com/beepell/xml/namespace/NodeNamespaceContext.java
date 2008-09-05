package com.beepell.xml.namespace;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;

import org.w3c.dom.Node;

/**
 * An implementation of NamespaceContext based on a single node.
 * 
 * @author Tim Hallwyl
 * 
 */
public class NodeNamespaceContext implements NamespaceContext {

    private final Node node;
    private final String defaultNameSpace;

    /**
     * Create a Namespace Context based on a specific node.
     * 
     * @param node the node used to resolve namespace declarations.
     */
    public NodeNamespaceContext(Node node) {
        this.node = node;
        this.defaultNameSpace = node.lookupNamespaceURI(null);
    }

    public String getNamespaceURI(String prefix) {
        if (prefix == null)
            throw new IllegalArgumentException("NamespaceContext.getNamespaceURI cannot resolve null");

        if (prefix.equals(XMLConstants.DEFAULT_NS_PREFIX))
            return this.defaultNameSpace;

        if (prefix.equals(XMLConstants.XML_NS_PREFIX))
            return XMLConstants.XML_NS_URI;

        if (prefix.equals(XMLConstants.XMLNS_ATTRIBUTE))
            return XMLConstants.XMLNS_ATTRIBUTE_NS_URI;

        String namespace = this.node.lookupNamespaceURI(prefix);
        if (namespace == null)
            return XMLConstants.NULL_NS_URI;

        return namespace;
    }

    public String getPrefix(String namespaceURI) {
        if (namespaceURI == null)
            throw new IllegalArgumentException("NamespaceContext.getPrefix cannot resolve null");

        if (namespaceURI.equals(this.defaultNameSpace))
            return XMLConstants.DEFAULT_NS_PREFIX;

        if (namespaceURI.equals(XMLConstants.XML_NS_URI))
            return XMLConstants.XML_NS_PREFIX;

        if (namespaceURI.equals(XMLConstants.XMLNS_ATTRIBUTE_NS_URI))
            return XMLConstants.XMLNS_ATTRIBUTE;

        return this.node.lookupPrefix(namespaceURI);
    }

    /**
     * Note: This method is not fully implemented. It does only return one
     * prefix of possible many.
     */
    public Iterator<String> getPrefixes(String namespaceURI) {
        // TODO Implement this in a reasonable way.
        List<String> list = new ArrayList<String>(1);
        list.add(getPrefix(namespaceURI));
        return list.iterator();
    }

}