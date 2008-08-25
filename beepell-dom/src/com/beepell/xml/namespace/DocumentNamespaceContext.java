package com.beepell.xml.namespace;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Map.Entry;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Namespace context to resolve namespace declarations. Please note: Because
 * this implementation is adding complete documents, either DOM or files, it
 * does not support shadowing, this is, overlapping, namespace declarations.
 * This comes naturally from the fact, that the interface does not know for
 * which node it is resolving names.
 * 
 * @author Tim Hallwyl, 2006, 2007
 */
public class DocumentNamespaceContext implements javax.xml.namespace.NamespaceContext {

    private String defaultNameSpace = XMLConstants.NULL_NS_URI;

    /**
     * List to lookup a namespace based on a prefix. That is, prefix is the key
     * and namespace is the value.
     */
    private Hashtable<String, String> namespaceURIs = new Hashtable<String, String>();

    /**
     * List to lookup a prefix based on an URI. That is, URI is the key and
     * prefix is the value.
     */
    private Hashtable<String, String> prefixes = new Hashtable<String, String>();

    /**
     * Create an empty namespace context based on DOM documents.
     * 
     */
    public DocumentNamespaceContext() {
        /* do nothing */
    }

    /**
     * Create a namespace context based on a document. This requires that the
     * document is using uniform namespace prefixes. That is, a namespace
     * prefix, for example &quot;bpel&quot;, may only be declared once.
     * According to the XML standard, namespace declarations may shadow, but
     * this is not supported using this approach.
     * 
     * @param document
     * @throws XPathExpressionException
     */
    public DocumentNamespaceContext(Document document) throws XPathExpressionException {
        add(document);
    }

    /**
     * Creates a namespace context bases on a file. The file is parsed and added
     * as a DOM tree. See NamespaceContext(Document document) for restrictions.
     * 
     * @param file
     * @throws XPathExpressionException
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws IOException
     */
    public DocumentNamespaceContext(File file) throws XPathExpressionException, ParserConfigurationException, SAXException, IOException {
        add(file);
    }

    /**
     * Gets the default namespace URI.
     * 
     * @return the default namespace URI
     */
    public String getDefaultNameSpaceURI() {
        return this.defaultNameSpace;
    }

    /**
     * Gets the prefix for the default namespace URI, if known. Otherwise it
     * returns null.
     * 
     * @return the default namespace prefix.
     */
    public String getDefaultNameSpacePrefix() {
        return this.prefixes.get(this.defaultNameSpace);
    }

    /**
     * @see javax.xml.namespace.NamespaceContext#getNamespaceURI(java.lang.String)
     */
    public String getNamespaceURI(String prefix) {

        if (prefix == null)
            throw new IllegalArgumentException("NamespaceContext.getNamespaceURI cannot resolve null");

        if (prefix.equals(XMLConstants.DEFAULT_NS_PREFIX))
            return this.defaultNameSpace;

        if (prefix.equals(XMLConstants.XML_NS_PREFIX))
            return XMLConstants.XML_NS_URI;

        if (prefix.equals(XMLConstants.XMLNS_ATTRIBUTE))
            return XMLConstants.XMLNS_ATTRIBUTE_NS_URI;

        String namespace = this.namespaceURIs.get(prefix);
        if (namespace == null)
            return XMLConstants.NULL_NS_URI;

        return namespace;
    }

    /**
     * @see javax.xml.namespace.NamespaceContext#getPrefix(java.lang.String)
     */
    public String getPrefix(String namespaceURI) {
        if (namespaceURI == null)
            throw new IllegalArgumentException("NamespaceContext.getPrefix cannot resolve null");

        if (namespaceURI.equals(this.defaultNameSpace))
            return XMLConstants.DEFAULT_NS_PREFIX;

        if (namespaceURI.equals(XMLConstants.XML_NS_URI))
            return XMLConstants.XML_NS_PREFIX;

        if (namespaceURI.equals(XMLConstants.XMLNS_ATTRIBUTE_NS_URI))
            return XMLConstants.XMLNS_ATTRIBUTE;

        return this.prefixes.get(namespaceURI);

    }

    /**
     * @see javax.xml.namespace.NamespaceContext#getPrefixes(java.lang.String)
     */
    public Iterator<String> getPrefixes(final String namespaceURI) {
        if (namespaceURI == null)
            throw new IllegalArgumentException("NamespaceContext.getPrefix cannot resolve null namespace");

        List<String> list = new ArrayList<String>();

        Set<Entry<String, String>> set = this.prefixes.entrySet();
        for (Entry<String, String> entry : set) {
            if (entry.getValue().equals(namespaceURI))
                list.add(entry.getKey());
        }

        return list.iterator();

    }

    /**
     * Add a single namespace prefix and URI to the context.
     * 
     * @param prefix
     * @param uri
     */
    public void add(String prefix, String uri) {
        if (prefix == null || uri == null)
            throw new IllegalArgumentException("NamespaceContext.add cannot add null");

        if (prefix.equals("xmlns") || prefix.equals(""))
            this.defaultNameSpace = uri;
        else {

            if (this.namespaceURIs.get(prefix) != null && !this.namespaceURIs.get(prefix).equals(uri))
                System.err.println("WARNING: Prefix " + prefix + ":" + uri + " shadows " + this.namespaceURIs.get(prefix));

            this.prefixes.put(uri, prefix);
            this.namespaceURIs.put(prefix, uri);
        }

    }

    /**
     * Adds all namespace declarations in the file to the context. Overlapping
     * prefixes will replace existing declarations.
     * 
     * @param file
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws IOException
     * @throws XPathExpressionException
     */
    public void add(File file) throws ParserConfigurationException, SAXException, IOException, XPathExpressionException {
        DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(true);
        DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
        Document document = documentBuilder.parse(file);
        add(document);
    }

    /**
     * Adds all namespace declarations in the document to the context.
     * Overlapping prefixes will replace existing declarations.
     * 
     * @param document
     * @throws XPathExpressionException
     */
    public void add(Document document) throws XPathExpressionException {

        XPathFactory xPathFactory = XPathFactory.newInstance();
        XPath xpath = xPathFactory.newXPath();
        NodeList result = (NodeList) xpath.evaluate("//namespace::*", document, XPathConstants.NODESET);

        if (result != null) {
            for (int i = 0; i < result.getLength(); i++) {
                String prefix = result.item(i).getLocalName();
                String uri = result.item(i).getNodeValue();
                this.add(prefix, uri);
            }
        }

    }
}
