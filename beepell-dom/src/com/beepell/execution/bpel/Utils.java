package com.beepell.execution.bpel;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.beepell.BPELConstants;

/**
 * @author Tim Hallwyl
 * 
 */
public class Utils {

    private static QName instanceQName = new QName(BPELConstants.BPI, "instance");

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

    /**
     * Returns true if the parent Element has one or more WS-BPEL child
     * activities.
     * 
     * @param element
     * @return true if the parent Element has one or more WS-BPEL child
     *         activities.
     */
    public static boolean hasChildActivities(Element element) {
        Node node = element.getFirstChild();
        while (node != null) {
            if (node instanceof Element && isActivity((Element) node))
                return true;
            node = node.getNextSibling();
        }
        return false;
    }

    /**
     * Returns true if the Element has a <targets> child Element.
     * 
     * @param element The parent element.
     * @return true if the Element has a <targets> child Element.
     */
    public static boolean hasTargets(Element element) {
        Node node = element.getFirstChild();
        while (node != null) {
            if ((node instanceof Element) && ("targets".equals(node.getLocalName())))
                return true;
            node = node.getNextSibling();
        }
        return false;
    }

    /**
     * Gets a list of linkNames that target this activity (Element).
     * 
     * @param element The Element to retrieve target linkNames from.
     * @return null if the Element has no targets, otherwise a List of
     *         linkNames.
     */
    public static List<String> getTargetsNames(Element element) {
        List<String> targets = new ArrayList<String>();

        Node node = element.getFirstChild();
        while (node != null) {
            if ((node instanceof Element) && ("targets".equals(node.getLocalName()))) {

                Node target = node.getFirstChild();
                while (target != null) {
                    if ((target instanceof Element) && ("target".equals(target.getLocalName()))) {
                        targets.add(((Element) target).getAttribute("linkName"));
                    }
                    target = target.getNextSibling();
                }

                return targets;
            }
            node = node.getNextSibling();
        }

        return null;
    }

    /**
     * Returns true if the Element is a WS-BPEL activity.
     * 
     * @param element The Element to check.
     * @return true if the Element is a WS-BPEL activity.
     */
    public static boolean isActivity(Element element) {
        if (!element.getNamespaceURI().equals(BPELConstants.BPI))
            return false;

        final String localName = element.getLocalName();
        if (localName.equals("assign"))
            return true;
        if (localName.equals("compensate"))
            return true;
        if (localName.equals("compensateScope"))
            return true;
        if (localName.equals("empty"))
            return true;
        if (localName.equals("exit"))
            return true;
        if (localName.equals("invoke"))
            return true;
        if (localName.equals("receive"))
            return true;
        if (localName.equals("reply"))
            return true;
        if (localName.equals("rethrow"))
            return true;
        if (localName.equals("throw"))
            return true;
        if (localName.equals("validate"))
            return true;
        if (localName.equals("wait"))
            return true;
        if (localName.equals("flow"))
            return true;
        if (localName.equals("forEach"))
            return true;
        if (localName.equals("if"))
            return true;
        if (localName.equals("pick"))
            return true;
        if (localName.equals("repeatUntil"))
            return true;
        if (localName.equals("scope"))
            return true;
        if (localName.equals("sequence"))
            return true;
        if (localName.equals("while"))
            return true;

        return false;
    }

    /**
     * Gets the first WS-BPEL child activity.
     * 
     * @param element The parent Element.
     * @return the first WS-BPEL child activity, or null if the parent has no
     *         children.
     */
    public static Element getFirstChildActivity(Element element) {
        Node node = element.getFirstChild();
        while (node != null) {
            if (node instanceof Element && isActivity((Element) node))
                return (Element) node;
            node = node.getNextSibling();
        }
        return null;
    }

    /**
     * Returns true if the Element has a <sources> child Element.
     * 
     * @param element
     * @return true if this element has a sources child.
     */
    public static boolean hasSources(Element element) {
        Node node = element.getFirstChild();
        while (node != null) {
            if ((node instanceof Element) && ("sources".equals(node.getLocalName())))
                return true;
            node = node.getNextSibling();
        }
        return false;
    }

    /**
     * Gets the join condition from an activity element.
     * 
     * @param element The activity element go get the join condition from.
     * @return The join condition expression string.
     */
    public static String getJoinCondition(Element element) {
        Node node = element.getFirstChild();
        while (node != null) {
            if ((node instanceof Element) && ("targets".equals(node.getLocalName()))) {

                Node child = node.getFirstChild();
                while (child != null) {
                    if ((child instanceof Element) && ("joinCondition".equals(child.getLocalName()))) {
                        return child.getTextContent().trim();
                    }
                    child = child.getNextSibling();
                }
            }
            node = node.getNextSibling();
        }
        return null;
    }
}
