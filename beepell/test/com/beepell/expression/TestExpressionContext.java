package com.beepell.expression;

import java.io.File;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import com.beepell.repository.SchemaRepository;
import com.beepell.xml.namespace.DocumentNamespaceContext;
import com.sun.xml.xsom.XSType;

/**
 * Dummy implementation of ExpressionContext for testing.
 * 
 * @author Tim Hallwyl
 */
public class TestExpressionContext implements ExpressionContext {

    private SchemaRepository repos;

    private DocumentBuilder builder;

    private String dmy = "http://beepell.com/samples/dummy/schema";

    private Document document;

    private XPath xpath;

    TestExpressionContext() {

        try {
            SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            Schema schema = schemaFactory.newSchema(new File("test/com/beepell/expression/schema.xsd"));

            repos = new SchemaRepository();
            repos.add(new File("./test/com/beepell/expression/schema.xsd").toURI());

            
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setSchema(schema);
            factory.setValidating(false);
            factory.setNamespaceAware(true);
            factory.setAttribute("http://apache.org/xml/properties/dom/document-class-name", "com.sun.org.apache.xerces.internal.dom.PSVIDocumentImpl");
            builder = factory.newDocumentBuilder();
            document = builder.parse("test/com/beepell/expression/document.xml");

            xpath = XPathFactory.newInstance().newXPath();
            xpath.setNamespaceContext(new DocumentNamespaceContext(document));

        } catch (Exception exception) {
            exception.printStackTrace();
        }

    }

    /**
     * Creates a new document with a clone of the given Node as the Document
     * Element. Returns the cloned node, not the new document.
     * 
     * @param node
     * @return the cloned node, not the new document.
     */
    private Node liberate(Node node) {
        boolean flag = false;

        if (node instanceof Text) {
            flag = true;
            node = node.getParentNode();
        }

        Document document = builder.newDocument();
        Node copy = document.importNode(node, true);
        document.appendChild(copy);
        if (flag)
            return copy.getFirstChild();
        else
            return copy;

    }

    public Boolean getLinkState(String link) {
        if (link.equals("tlink"))
            return true;
        if (link.equals("flink"))
            return false;
        // Otherwise, if unset, return null:
        return null;
    }

    public XSType getType(QName qName) {

        try {
            
            return repos.getType(qName);
            
        } catch (Exception exception) {
            exception.printStackTrace();
            return null;
        }

    }

    public QName getVariablePropertyType(QName property) {
        
        return getVariableType(property.getLocalPart());
        
    }

    public Node getVariablePropertyValue(String variable, QName property) {
        
        return getVariableValue(property.getLocalPart());
    }

    public QName getVariableType(String variable) {

        if (variable.contains(".")) {
            variable = variable.substring(variable.indexOf('.') + 1);
        }
        
        if (variable.equals("person"))
            return new QName(dmy, "person");

        if (variable.equals("personinfo"))
            return new QName(dmy, "personinfo");

        if (variable.equals("tBln"))
            return new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "boolean");

        if (variable.equals("fBln"))
            return new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "boolean");

        if (variable.equals("flt"))
            return new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "float");

        if (variable.equals("ntgr"))
            return new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "int");

        if (variable.equals("untgr"))
            return new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "unsignedInt");

        if (variable.equals("dt"))
            return new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "date");

        return new QName("http://beepell.com/bogus", "paqtest");
    }

    public QName getVariableType(String variable, String part) {

        return getVariableType(part);
    }

    public Node getVariableValue(String variable) {
        try {
            if (variable.equals("person"))
                return liberate((Node) xpath.evaluate("/dmy:foo/dmy:person", document, XPathConstants.NODE));

            if (variable.equals("personinfo"))
                return liberate((Node) xpath.evaluate("/dmy:foo/dmy:employee", document, XPathConstants.NODE));

            if (variable.equals("tBln")) {
                Node node = (Node) xpath.evaluate("/dmy:foo/dmy:tBln/text()", document, XPathConstants.NODE);
                return liberate(node);
            }

            if (variable.equals("fBln"))
                return liberate((Node) xpath.evaluate("/dmy:foo/dmy:fBln/text()", document, XPathConstants.NODE));

            if (variable.equals("flt"))
                return liberate((Node) xpath.evaluate("/dmy:foo/dmy:flt/text()", document, XPathConstants.NODE));

            if (variable.equals("ntgr"))
                return liberate((Node) xpath.evaluate("/dmy:foo/dmy:ntgr/text()", document, XPathConstants.NODE));

            if (variable.equals("untgr"))
                return liberate((Node) xpath.evaluate("/dmy:foo/dmy:untgr/text()", document, XPathConstants.NODE));

            if (variable.equals("dt"))
                return liberate((Node) xpath.evaluate("/dmy:foo/dmy:dt/text()", document, XPathConstants.NODE));

            return null;

        } catch (Exception exception) {

            exception.printStackTrace();
            return null;

        }
    }

    public Node getVariableValue(String variable, String part) {

        return getVariableValue(part);
    }

}
