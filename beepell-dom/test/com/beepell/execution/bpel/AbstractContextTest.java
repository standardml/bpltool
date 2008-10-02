package com.beepell.execution.bpel;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.beepell.xml.namespace.DocumentNamespaceContext;


/**
 * Contains common method for tests on the Context.
 * 
 * @author Tim Hallwyl
 * 
 */
public class AbstractContextTest {

    private Element instance;

    private XPath xPath;

    protected Element getInstance() {
        return this.instance;
    }
    
    protected Node evaluate(final String expression, final Node contextNode) throws XPathExpressionException {
        return (Node) this.xPath.evaluate(expression, contextNode, XPathConstants.NODE);
    }

    protected Object evaluate(final String expression, final Node contextNode, final javax.xml.namespace.QName returnType) throws XPathExpressionException {
        return this.xPath.evaluate(expression, contextNode, returnType);
    }
    
    /**
     * Load instance tree from file.
     * 
     * @param file The file to load.
     * @throws IOException If the file could not be loaded.
     */
    protected void load(File file) throws IOException {
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
            if (!Utils.isInstanceElement(element))
                throw new IOException("File does not contain a process instance tree.");
            
            this.instance = element;
            XPathFactory xPathFactory = XPathFactory.newInstance();
            this.xPath = xPathFactory.newXPath();
            this.xPath.setNamespaceContext(new DocumentNamespaceContext(this.instance.getOwnerDocument()));

        } catch (SAXException exception) {
            throw new IOException(exception.getLocalizedMessage(), exception);
        } catch (ParserConfigurationException exception) {
            throw new IOException(exception.getLocalizedMessage(), exception);
        } catch (XPathExpressionException exception) {
            throw new IOException(exception.getLocalizedMessage(), exception);
        }

    }

    /**
     * Uses a transformer serializer to get a string representation of the node.
     * 
     * @param node Node to get as String
     * @return XML fragment string.
     */
    public static String toString(Node node) {
        if (node == null)
            return "null-node";

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

}
