package com.beepell.util;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.StringWriter;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.validation.Schema;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import com.beepell.tools.explorer.XPathExplorer;
import com.beepell.xml.namespace.DocumentNamespaceContext;;

/**
 * XML Utilities.
 * 
 * @author Tim Hallwyl
 * 
 */
public class XML {

    /**
     * A quick XPath evaluation method.
     * TODO: Delete this method at some point.
     * @param document
     * @param expression
     * @return a node list of the selected nodes.
     * @throws XPathExpressionException
     */
    public static NodeList xpath(Document document, String expression) throws XPathExpressionException {
        
        XPath xpath = XPathFactory.newInstance().newXPath();
        xpath.setNamespaceContext(new DocumentNamespaceContext(document));
        return (NodeList) xpath.evaluate(expression, document, XPathConstants.NODESET);        
        
    }
    
    /**
     * Reads an XML file into a DOM Document and validates against an XML Schema.
     * @param file
     * @param schema
     * @return W3C DOM Document representing the XML file content.
     * @throws ParserConfigurationException
     * @throws IOException
     * @throws SAXException
     */
    public static Document read(File file, Schema schema) throws ParserConfigurationException, IOException, SAXException {
        
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setSchema(schema);
        factory.setValidating(false);
        factory.setCoalescing(false);
        factory.setIgnoringComments(true);
        factory.setIgnoringElementContentWhitespace(true);
        factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, false);
        factory.setNamespaceAware(true);
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.parse(file);
        document.normalizeDocument();
        return document;
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
        } else {
            uri = node.lookupNamespaceURI(split[0]);
            return new QName(uri, split[1], split[0]);
        }
    }
    

    /**
     * Uses a transformer serializer to get a string representation of the node.
     * @param node Node to get as String
     * @return XML fragment string.
     */
    public static String toString(Node node) {
        if (node == null)
            return "null-node";
        
        if (node.getNodeValue() != null) {
            XPathExplorer.log(node.getLocalName() + " = " + node.getNodeValue());
        }
        
        try {
        StringWriter sw = new StringWriter();
        Transformer serializer;
        serializer = TransformerFactory.newInstance().newTransformer();
        serializer.setOutputProperty(OutputKeys.INDENT, "yes");
        serializer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
        serializer.setOutputProperty(OutputKeys.METHOD, "xml");
        serializer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
        serializer.transform(new DOMSource(node), new StreamResult(sw));
        return sw.toString();
        } catch (Exception exception) {
            return "Failed to parse node to String.";
        } 
    }
    
    /**
     * Constructs a DOM document from a xml fragment string.
     * @param string
     * @return A DOM Document node.
     * @throws SAXException
     * @throws IOException
     * @throws ParserConfigurationException
     */
    public static Node toNode(String string) throws SAXException, IOException, ParserConfigurationException {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        return dbf.newDocumentBuilder().parse(new ByteArrayInputStream(string.getBytes()));
    }

    /**
     * Returns an back path from node to document.
     * @param node
     * @return an XPath like string
     */
    public static String toPath(Node node) {
        
        if (node == null)
            return "";
        
        if (node instanceof Document) {
            return "";
        }
        
        if (node instanceof Attr) {
            return toPath(node.getParentNode()) + "/@" + ((Attr) node).getLocalName();
        }
        
        if (node instanceof Element) {
            return toPath(node.getParentNode()) + "/" + ((Element) node).getLocalName();
        }
        
        if (node instanceof Text) {
            return toPath(node.getParentNode());
        }
        
        return toPath(node.getParentNode());
    }
    
}
