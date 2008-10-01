package com.beepell.execution.bpel;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

/**
 * Contains common method for tests on the Context.
 * 
 * @author Tim Hallwyl
 *
 */
public class AbstractContextTest {

    protected Element instance;
    
    protected XPath xPath;
    
    protected Node evaluate(final String expression, final Node contextNode) throws XPathExpressionException {
        return (Node) this.xPath.evaluate(expression, contextNode, XPathConstants.NODE);
    }
    
    /**
     * Load instance tree from file.
     * 
     * @param file The file to load.
     * @return The instance Element node.
     * @throws IOException If the file could not be loaded.
     */
    protected static Element load(File file) throws IOException {
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

            return element;

        } catch (SAXException exception) {
            throw new IOException(exception.getLocalizedMessage(), exception);
        } catch (ParserConfigurationException exception) {
            throw new IOException(exception.getLocalizedMessage(), exception);
        }

    }
    
    
}
