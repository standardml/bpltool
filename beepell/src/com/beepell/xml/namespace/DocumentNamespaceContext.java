package com.beepell.xml.namespace;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

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

    private Hashtable<String, String> namespaceURIs = new Hashtable<String, String>();

    private Hashtable<String, String> prefixes = new Hashtable<String, String>();
    
    /**
     * Create an empty namespace context based on DOM documents.
     *
     */
    public DocumentNamespaceContext() {
        
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
        return defaultNameSpace;
    }

    /**
     * Gets the prefix for the default namespace URI, if known. Otherwise it
     * returns null.
     * 
     * @return the default namespace prefix.
     */
    public String getDefaultNameSpacePrefix() {
        return prefixes.get(defaultNameSpace);
    }

    /**
     * @see javax.xml.namespace.NamespaceContext#getNamespaceURI(java.lang.String)
     */
    public String getNamespaceURI(String prefix) {

        if (prefix == null)
            throw new IllegalArgumentException("NamespaceContext.getNamespaceURI cannot resolve null");

        if (prefix.equals(XMLConstants.DEFAULT_NS_PREFIX))
            return defaultNameSpace;

        if (prefix.equals(XMLConstants.XML_NS_PREFIX))
            return XMLConstants.XML_NS_URI;

        if (prefix.equals(XMLConstants.XMLNS_ATTRIBUTE))
            return XMLConstants.XMLNS_ATTRIBUTE_NS_URI;

        String namespace = namespaceURIs.get(prefix);
        if (namespace == null)
            return XMLConstants.NULL_NS_URI;
        else
            return namespace;
    }

    /**
     * @see javax.xml.namespace.NamespaceContext#getPrefix(java.lang.String)
     */
    public String getPrefix(String namespaceURI) {
        if (namespaceURI == null)
            throw new IllegalArgumentException("NamespaceContext.getPrefix cannot resolve null");

        if (namespaceURI.equals(defaultNameSpace))
            return XMLConstants.DEFAULT_NS_PREFIX;

        if (namespaceURI.equals(XMLConstants.XML_NS_URI))
            return XMLConstants.XML_NS_PREFIX;

        if (namespaceURI.equals(XMLConstants.XMLNS_ATTRIBUTE_NS_URI))
            return XMLConstants.XMLNS_ATTRIBUTE;

        return prefixes.get(namespaceURI);

    }

    /**
     * @see javax.xml.namespace.NamespaceContext#getPrefixes(java.lang.String)
     */
    public Iterator getPrefixes(final String namespaceURI) {
        if (namespaceURI == null)
            throw new IllegalArgumentException("NamespaceContext.getPrefix cannot resolve null");

        Set<Map.Entry<String, String>> set = namespaceURIs.entrySet();
        ArrayList<Map.Entry<String, String>> list = new ArrayList<Map.Entry<String, String>>();
        for (Map.Entry<String, String> entry : set) {
            if (entry.getValue().equals(namespaceURI))
                list.add(entry);
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

            if (namespaceURIs.get(prefix) != null && !namespaceURIs.get(prefix).equals(uri))
                System.err.println("WARNING: Prefix " + prefix + ":" + uri + " shadows " + namespaceURIs.get(prefix));

            prefixes.put(uri, prefix);
            namespaceURIs.put(prefix, uri);
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
