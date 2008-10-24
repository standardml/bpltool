package com.beepell.xml.xpath;

import java.io.File;
import java.io.StringWriter;
import java.net.URI;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.xpath.XPathFunction;
import javax.xml.xpath.XPathFunctionException;
import javax.xml.xpath.XPathFunctionResolver;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import com.beepell.BPELConstants;
import com.beepell.exceptions.XsltInvalidSource;
import com.beepell.execution.bpel.Context;

/**
 * An implementation of XPathFunctionResolver based on ExpressionContext.
 * 
 * @author Tim Hallwyl
 */
public class FunctionResolver extends Resolver implements XPathFunctionResolver {

    private final NamespaceContext namespaceContext;

    /**
     * The WS-BPEL specification is not clear on, if the result of
     * getVariableProperty in an XPath expression should be converted as with
     * variables. We set default to "yes" they must be converted, as we believe
     * this is the intension.
     */
    protected boolean convertPropertyValues;

    /**
     * Create a FunctionResolver based on an ExpressionContext.
     * 
     * @param context Expression context used to resolve variables and
     *            properties.
     * @param namespaceContext Namespace context used to qualify property names.
     * @throws SAXException
     */
    public FunctionResolver(Context context, NamespaceContext namespaceContext) throws SAXException {
        super(context);
        this.namespaceContext = namespaceContext;
        this.convertPropertyValues = false;
    }

    public XPathFunction resolveFunction(QName functionName, int arity) {
        if (functionName.getNamespaceURI().equals(BPELConstants.BPEL)) {

            if (functionName.getLocalPart().equals("getVariableProperty")) {
                return new XPathFunction() {

                    @SuppressWarnings("unchecked")
                    public Object evaluate(List args) throws XPathFunctionException {
                        try {
                            String variable = (String) args.get(0);
                            QName property = toQName((String) args.get(1));
                            Node value = FunctionResolver.this.context.getVariablePropertyValue(variable, property);
                            if (FunctionResolver.this.convertPropertyValues)
                                return toXPathObject(value, FunctionResolver.this.context.getVariablePropertyType(property));
                            else
                                return value;
                        } catch (Exception exception) {
                            throw new XPathFunctionException(exception);
                        }
                    }
                };
            }

            if (functionName.getLocalPart().equals("doXslTransform")) {
                return new XPathFunction() {

                    @SuppressWarnings("unchecked")
                    public Object evaluate(List args) throws XPathFunctionException {
                        try {
                            if (!(args.get(1) instanceof NodeList))
                                throw new XsltInvalidSource("Source parameter is not a XPath node-set.");

                            if (((NodeList) args.get(1)).getLength() != 1)
                                throw new XsltInvalidSource("Source node-set must contain a single element node. (" + ((NodeList) args.get(1)).getLength()
                                        + ")");

                            if (!(((NodeList) args.get(1)).item(0) instanceof Element))
                                throw new XsltInvalidSource("Source node-set does not contain an element node.");

                            if (!(args.get(0) instanceof String))
                                throw new IllegalArgumentException("Style sheet URI must be a string literal.");

                            URI styleSheetURI = new URI((String) args.get(0));
                            Element source = (Element) ((NodeList) args.get(1)).item(0);

                            Object[] properties = new Object[args.size() - 2];
                            for (int i = 2; i < args.size(); i++)
                                properties[i - 2] = args.get(i);

                            return doXslTransform(styleSheetURI, source, properties);

                        } catch (Exception exception) {
                            throw new XPathFunctionException(exception);
                        }
                    }
                };
            }

        }

        return null;
    }

    protected QName toQName(String name) {
        // TODO: Is there an QName string representation with complete URI that
        // should be supported?

        String string = name.trim();

        // QNames of the form "xsd:boolean"
        int colon = string.indexOf(':');
        if (colon >= 0) {
            String prefix = string.substring(0, string.indexOf(':'));
            String localPart = string.substring(string.indexOf(':') + 1);
            String namespaceURI = this.namespaceContext.getNamespaceURI(prefix);
            return new QName(namespaceURI, localPart, prefix);
        }

        // QNames of the form "{http://www.w3.org/2001/XMLSchema}boolean"
        int braceOpen = string.indexOf('{');
        int braceClose = string.indexOf('}');
        if (braceOpen >= 0 && braceClose > braceOpen) {
            String localPart = string.substring(braceClose + 1);
            String namespaceURI = string.substring(braceOpen + 1, braceClose);
            String prefix = this.namespaceContext.getPrefix(namespaceURI);
            return new QName(namespaceURI, localPart, prefix);
        }

        return new QName(null, string, XMLConstants.DEFAULT_NS_PREFIX);
    }

    /**
     * @param style An URI naming the style sheet to be used.
     * @param sourceElement An XPath node set providing the source document for
     *            the transformation to be performed. This set MUST contain a
     *            single EII (i.e. an element node in XPath 1.0 data model). The
     *            single EII as specified by this parameter will be treated as
     *            the single child of the root node of the source tree for XSLT
     *            processing.
     * @param properties This optional parameter MUST appear in pairs. Each pair
     *            is defined as: an XPath string parameter providing the
     *            qualified name of an XSLT parameter, an XPath object parameter
     *            providing the value for the named XSLT parameter.
     * @return The result of the transformation. The result is one of the
     *         following infoset items, depending on the XSLT output method
     *         employed by the selected style sheet: A single TII (an XPath 1.0
     *         text node), created by the XSLT "text" or "html" output methods,
     *         or A single EII (an XPath element node that is the single child
     *         of the root of the result tree), which is created by the XSLT
     *         "xml" output method.
     * @throws TransformerException if the transformation fails.
     * @throws XsltInvalidSource if the source is not an element node (or a
     *             node-set with a single element node)
     */
    protected Node doXslTransform(URI style, Element sourceElement, Object[] properties) throws TransformerException, XsltInvalidSource {

        File file = new File(style);
        if (!file.canRead())
            throw new XsltInvalidSource("Cannot find style sheet: " + style.toString());

        DOMSource source = new DOMSource(sourceElement);
        TransformerFactory factory = TransformerFactory.newInstance();
        Transformer transformer = factory.newTransformer(new StreamSource(file));
        String method = transformer.getOutputProperty("method");

        for (int i = 0; i < properties.length; i = i + 2)
            transformer.setParameter((String) properties[i], properties[i + 1]);

        if (method.equals("xml")) {
            DOMResult result = new DOMResult();
            transformer.transform(source, result);
            return ((Document) result.getNode()).getDocumentElement();
        } else {
            StringWriter writer = new StringWriter();
            StreamResult result = new StreamResult(writer);
            transformer.transform(source, result);
            try {
                Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
                Text text = document.createTextNode(writer.toString());
                Element element = document.createElement("text");
                element.appendChild(text);
                document.appendChild(element);
                return text;
            } catch (Exception exception) {
                throw new TransformerException(exception);
            }
        }

    }

}
