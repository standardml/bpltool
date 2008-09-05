package com.beepell.repository;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Hashtable;
import java.util.Iterator;

import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * A container class to pool together schema definitions. This is used by the
 * SchemaRepository.
 * 
 * @author Tim Hallwyl
 */
public class SchemaPool {

    private final SchemaFactory factory;

    private final Hashtable<QName, Schema> schemas;

    private final DocumentBuilder builder;

    private final XPath xPath;

    /**
     * @throws ParserConfigurationException
     */
    public SchemaPool() throws ParserConfigurationException {
        this.schemas = new Hashtable<QName, Schema>();
        this.factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        this.factory.setErrorHandler(new ErrorHandler());
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        this.builder = dbf.newDocumentBuilder();

        this.xPath = XPathFactory.newInstance().newXPath();
        this.xPath.setNamespaceContext(new NamespaceContext() {

            public String getNamespaceURI(String prefix) {
                return XMLConstants.W3C_XML_SCHEMA_NS_URI;
            }

            public String getPrefix(String namespaceURI) {
                return "xsd";
            }

            public Iterator<String> getPrefixes(String namespaceURI) {
                return null;
            }

        });
    }

    /**
     * @param document
     */
    public void add(Document document) {
        try {
            Source source = new DOMSource(document);
            Schema schema = this.factory.newSchema(source);

            String namespace = this.xPath.evaluate("/xsd:schema/@targetNamespace", document);

            NodeList names = (NodeList) this.xPath.evaluate("//xsd:*/@name", document, XPathConstants.NODESET);

            QName qName;
            for (int i = 0; i < names.getLength(); i++) {
                Attr name = (Attr) names.item(i);
                qName = new QName(namespace, name.getValue());
                this.schemas.put(qName, schema);
            }

        } catch (SAXException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (XPathExpressionException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * @param uri
     */
    public void add(URI uri) {

        try {
            Document document = this.builder.parse(uri.toString());

            add(document);

        } catch (SAXException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    /**
     * 
     * @param stream
     */
    public void add(InputStream stream) {

        try {
            Document document;
            document = this.builder.parse(stream);
            add(document);
        } catch (SAXException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    /**
     * @param type
     * @return schema declaring the type or null.
     */
    public Schema getSchema(QName type) {
        if (type == null)
            throw new NullPointerException("getSchema: QName type must not be null.");

        /*
         * For basic XML Schema type any schema can be used.
         */
        if (type.getNamespaceURI().equals(XMLConstants.W3C_XML_SCHEMA_NS_URI)) {
            try {
                return this.factory.newSchema();
            } catch (SAXException exception) {
                exception.printStackTrace();

            }
        }

        return this.schemas.get(type);
    }

}
