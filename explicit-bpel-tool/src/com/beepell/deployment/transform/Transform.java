package com.beepell.deployment.transform;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Iterator;

import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * Command-line toolkit to transform WS-BPEL into Core BPEL.
 * 
 * @author Tim Hallwyl, IT-University of Copenhagen, 2008 - 2010
 * @author Espen H¿jsgaard, IT-University of Copenhagen, 2006 - 2010
 */
public class Transform {

    private static final String bpelURI = "http://docs.oasis-open.org/wsbpel/2.0/process/executable";
    private final TransformerFactory transformerFactory = TransformerFactory.newInstance();
    private final SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
    private final Schema bpelSchema;
    private final Schema cBpelSchema;
    private final boolean verbose;
    private String[] sheets = { 
            "remove-documentation.xsl",                  // Remove human readable documentation
            "remove-extension-activity.xsl",             // Remove extension activities
            "remove-extension-assign.xsl",               // Remove extension assignments
            "remove-extension-attributes-elements.xsl",  // Remove extension attributes and elements
            "remove-extension-declarations.xsl",         // Remove extension declarations
            "default-message-exchanges.xsl",             // Make default message exchanges explicit
            "process.xsl",                               // Move process scope to an explicit scope
            "while.xsl",                                 // while to repeatUntil
    		"if.xsl",                                    // Add missing else-clause and turn elseif into nested if
    		"scope.xsl",                                 // Variable initializations
            "receive.xsl",                               // Change <receive>s into <pick>s
            "invoke.xsl",
            "pick.xsl",
            "reply.xsl",
            "default-handlers.xsl",
            "sequence.xsl",                              // Change <sequence>s into <flow>s
            "standard-attributes-elements.xsl",          // Move standard attributes and elements to wrapper <flow>s
            "default-conditions.xsl",
            "default-attribute-values-simple.xsl",
            "default-attribute-values-global.xsl", 
            "default-attribute-values-inherited.xsl", 
            "remove-redundant-attributes.xsl"
            };
    
    private boolean validate = true;

    public Transform() throws SAXException {
        this.bpelSchema = this.schemaFactory.newSchema(new File("schemas/bpel.xsd"));
        this.cBpelSchema = this.schemaFactory.newSchema(new File("schemas/c-bpel.xsd"));
        this.verbose = true;
    }

    public Document transform(File source) throws TransformerException {
        Document bpel = null, cbpel = null;
        try {
            String uniquePrefix = getUniquePrefix(source);
            System.out.println("Unique Prefix: " + uniquePrefix);

            bpel = parse(source, this.bpelSchema);
            cbpel = transform(bpel, uniquePrefix);
            
            if (this.validate) {
              System.out.println("Validating that transformed process description is valid WS-BPEL.");
              this.validate(cbpel, this.bpelSchema);
              System.out.println("Transformed process description is valid WS-BPEL.");
              System.out.println("Validating that transformed process description is valid Core BPEL.");
              this.validate(cbpel, this.cBpelSchema);
              System.out.println("Transformed process description is valid Core BPEL.");
            }
            
            if (this.verbose)
                System.out.println(toString(cbpel));

            return cbpel;

        } catch (TransformerException exception) {
            throw exception;
        } catch (Exception exception) {
            System.out.println(toString(cbpel));
            throw new TransformerException(exception.getLocalizedMessage(), exception);
        }
    }

    public void transform(File source, File target) throws TransformerException {
        Document ebpel = null;
        try {

            ebpel = transform(source);
            serialize(ebpel, target);

        } catch (TransformerException exception) {
            serialize(ebpel, target);
            throw exception;
        } catch (Exception exception) {
            serialize(ebpel, target);
            throw new TransformerException(exception.getLocalizedMessage(), exception);
        }
    }

    /**
     * Transforms a WS-BPEL Document into an Core BPEL Document.
     * 
     * @param document The WS-BPEL Document.
     * @param uniquePrefix The prefix used when generating variable and link
     *            names. This must be unique within the original document.
     * @return The E-BPEL Document.
     * @throws TransformerException If the transformation fails.
     */
    private Document transform(Document document, String uniquePrefix) throws TransformerException {
        try {
            String systemId = document.getBaseURI().toString();

            Transformer transformer;
            DOMSource source = new DOMSource(document, systemId);
            DOMResult result = null;

            for (int index = 0; index < sheets.length; index++) {

                System.out.println("Applying transformation 'xslt" + File.separator + sheets[index] + "'.");
                transformer = getTransformer(sheets[index]);
                transformer.setParameter("uniquePrefix", uniquePrefix);
                result = new DOMResult();
                transformer.transform(source, result);
                source = new DOMSource(result.getNode(), systemId);

            }

            if (result == null)
                throw new TransformerException("Transformation returned a 'null' result.");

            return (Document) result.getNode();

        } catch (TransformerException exception) {
            throw exception;
        } catch (Exception exception) {
            throw new TransformerException(exception.getLocalizedMessage(), exception);
        }

    }

    private Transformer getTransformer(String filename) throws TransformerConfigurationException, TransformerFactoryConfigurationError, MalformedURLException {
        URL url = new File("xslt" + File.separator + filename).toURI().toURL();
        StreamSource source = new StreamSource(url.toString());
        return this.transformerFactory.newTransformer(source);
    }

    /**
     * Command-line tool kit for transformation of WS-BPEL into Core BPEL.
     * <p>
     * Usage: Transform bpel [target]
     * <ul>
     * <li>bpel - the WS-BPEL source file</li>
     * <li>target (optional) - the Core BPEL target file</li>
     * </ul>
     * <p>
     * When no target file is specified a file with same name and location, but
     * with .ebpel postfix, is created.
     * 
     * @param args
     */
    public static void main(String[] args) {
        try {
            if (args.length < 1) {
                System.err.println("Usage: Transform bpel [target]");
                System.exit(-1);
            }

            final File source = new File(args[0]);

            File target;
            if (args.length < 2) {
                String fileName = source.getCanonicalPath().substring(0, args[0].lastIndexOf('.')) + ".ebpel";
                target = new File(fileName);
            } else {
                target = new File(args[1]);
            }

            if (!source.canRead())
                throw new IOException("Failed to read from source file: " + source.getAbsolutePath());

            target.createNewFile();
            if (!target.exists() || !target.canWrite())
                throw new IOException("Cannot write to target file: " + target.getAbsolutePath());

            new Transform().transform(source, target);

        } catch (Exception exception) {
            System.err.println("Transformation failed: " + exception.getLocalizedMessage());
            exception.printStackTrace();
            System.exit(-1);
        }

    }

    /**
     * Validates a W3C DOM Document against a Schema.
     * 
     * @param document The Document to be validated.
     * @param schema The Schema to validate against.
     * @throws SAXException If any validation errors occurs.
     * @throws IOException If the validator is processing a SAXSource and the
     *             underlying org.xml.sax.XMLReader throws an IOException.
     */
    private void validate(final Document document, final Schema schema) throws SAXException, IOException {
        DOMResult result = null;
        schema.newValidator().validate(new DOMSource(document), result);
    }

    /**
     * Writes out a W3C DOM Document to an XML file.
     * 
     * @param document W3C DOM Document source.
     * @param file XML File target.
     * @throws TransformerFactoryConfigurationError If the transformer could not
     *             be configured.
     * @throws TransformerException If the Document could not be serialized.
     */
    private void serialize(Document document, File file) throws TransformerFactoryConfigurationError, TransformerException {
        Result target = new StreamResult(file);
        Transformer transformer = TransformerFactory.newInstance().newTransformer();
        transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
        transformer.setOutputProperty(OutputKeys.INDENT, "yes");
        transformer.transform(new DOMSource(document), target);
    }

    /**
     * Parses an XML file into a W3C DOM Document object and validate it. If
     * schema is null, the file is parsed without validation.
     * 
     * @param file Source file to parse and validate.
     * @param schema Schema to validate source against.
     * @return A W3C DOM Document object
     * @throws ParserConfigurationException If the parser could not be
     *             configured.
     * @throws SAXException If any parse or validation errors occur.
     * @throws IOException If the file could not be read.
     */
    private Document parse(final File file, final Schema schema) throws ParserConfigurationException, SAXException, IOException {
        final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setSchema(schema);
        documentBuilderFactory.setValidating(false);
        documentBuilderFactory.setCoalescing(false);
        documentBuilderFactory.setIgnoringComments(true);
        documentBuilderFactory.setIgnoringElementContentWhitespace(true);
        documentBuilderFactory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, false);
        documentBuilderFactory.setNamespaceAware(true);

        final DocumentBuilder builder = documentBuilderFactory.newDocumentBuilder();
        final Document document = builder.parse(file);
        document.normalizeDocument();

        if (schema != null)
            this.validate(document, schema);

        return document;
    }

    /**
     * Get a unique prefix for both variable and link names.
     * 
     * @param document
     */
    @SuppressWarnings("unchecked")
    private static String getUniquePrefix(File file) throws Exception {
        final String prefixBase = "v";
        int prefixCount = 0;

        try {
            DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
            documentBuilderFactory.setNamespaceAware(true);
            DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
            Document document = documentBuilder.parse(file);
            NamespaceContext namespaceContext = new DocumentNamespaceContext(document);

            String bpel = "";
            Iterator<String> iterator = namespaceContext.getPrefixes(bpelURI);
            while (iterator.hasNext() && bpel.isEmpty()) {
                bpel = iterator.next();
            }

            if (bpel.isEmpty()) {
                throw new Exception("Unable to find a prefix for WS-BPEL namespace.");
            }

            XPathFactory factory = XPathFactory.newInstance();
            XPath xPath = factory.newXPath();
            xPath.setNamespaceContext(namespaceContext);
            NodeList variables = (NodeList) xPath.evaluate("//" + bpel + ":variable/@name | //" + bpel + ":link/@name", document, XPathConstants.NODESET);

            // Build String array with all names
            String[] names = new String[variables.getLength()];
            Node node;
            for (int i = 0; i < variables.getLength(); i++) {
                node = variables.item(i);
                names[i] = node.getNodeValue();
            }

            // Sort String array with names
            Arrays.sort(names);

            // Find unique prefix
            String prefix = prefixBase + prefixCount;
            for (int i = 0; i < names.length; i++) {
                if (names[i].startsWith(prefix)) {
                    prefix = prefixBase + ++prefixCount;
                }
            }

            return prefix;

        } catch (Exception exception) {

            System.err.println("Failed to generate an unique prefix: " + exception.getLocalizedMessage());
            exception.printStackTrace();
        }
        return null;
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

    /**
     * Set an alternative list of style sheets to be used.
     * 
     * @param sheets An ordered String array of style sheet filenames to be
     *            used.
     */
    public void setSheets(String[] sheets) {
        this.sheets = sheets;
    }
    
    /**
     * Get the current list of style sheets.
     * 
     * @return a list of style sheet file names.
     */
    public String[] getSheets() {
        return sheets;
    }

    /**
     * Sets if the transformer should validate the result against the Core BPEL
     * schema. Default is true. Used for testing.
     * 
     * @param validate true if the transformer should validate the result.
     */
    public void setValidate(boolean validate) {
        this.validate = validate;
    }

    /**
     * Sets if the transformer should validate the result against the Core BPEL
     * schema. Default is true. Used for testing.

     * @return true if the transformer validates the result.
     */
    public boolean getValidate() {
        return this.validate;
    }

}
