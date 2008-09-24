package com.beepell.execution.bpel;

import java.io.File;
import java.io.IOException;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

/**
 * @author Tim Hallwyl
 * 
 */
public class Utils {

    private static QName instanceQName = new QName("http://www.itu.dk/research/pls/bpl/bpel/instance", "instance");

    /**
     * Load instance tree from file.
     * 
     * @param file The file to load.
     * @return The instance Element node.
     * @throws IOException If the file could not be loaded.
     */
    public static Element load(File file) throws IOException {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setValidating(false);
            factory.setCoalescing(false);
            factory.setIgnoringComments(true);
            factory.setIgnoringElementContentWhitespace(true);
            factory.setNamespaceAware(true);
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document document;

            document = builder.parse(file);
            document.normalizeDocument();
            Element element = document.getDocumentElement();
            if (!isInstanceElement(element))
                throw new IOException("File does not contain a process instance tree.");

            return element;

        } catch (SAXException exception) {
            throw new IOException(exception.getLocalizedMessage(), exception);
        } catch (ParserConfigurationException exception) {
            throw new IOException(exception.getLocalizedMessage(), exception);
        }

    }

    /**
     * Returns true if the Element is an PBL instance Element.
     * 
     * @param element The Element to check.
     * @return true if the Element is an PBL instance Element.
     */
    public static boolean isInstanceElement(Element element) {
        if (element.getNamespaceURI().equals(instanceQName.getNamespaceURI()) && element.getLocalName().equals(instanceQName.getLocalPart()))
            return true;
        return false;
    }

    /**
     * Gets the first child Element node.
     * 
     * @param element The parent Element.
     * @return the first child Element, or null if the parent has no children.
     */
    public static Element getFirstChildElement(Element element) {
        Node node = element.getFirstChild();
        while (node != null) {
            if (node instanceof Element)
                return (Element) node;
            node = node.getNextSibling();
        }
        return null;
    }


}
