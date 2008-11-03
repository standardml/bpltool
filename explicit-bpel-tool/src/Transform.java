import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.XMLConstants;
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

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * Command-line toolkit to transform WS-BPEL into E-BPEL.
 * 
 * @author Tim Hallwyl, IT-University of Copenhagen, 2008
 */
public class Transform {

    private final TransformerFactory transformerFactory = TransformerFactory.newInstance();
    private final SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
    private final Schema bpelSchema;
    private final Schema ebpelSchema;

    private Transform() throws SAXException {
        this.bpelSchema = this.schemaFactory.newSchema(new File("schemas/bpel.xsd"));
        this.ebpelSchema = this.schemaFactory.newSchema(new File("schemas/e-bpel.xsd"));

    }

    private void transform(File source, File target) throws TransformerException {
        Document bpel = null, ebpel = null;
        try {
            
            bpel = parse(source, this.bpelSchema);
            ebpel = transform(bpel);
            this.validate(ebpel, this.bpelSchema);
            System.out.println("Transformed process description is valid WS-BPEL.");
            this.validate(ebpel, this.ebpelSchema);
            System.out.println("Transformed process description is valid E-BPEL.");

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
     * Transforms a WS-BPEL Document into an E-BPEL Document.
     * 
     * @param document The WS-BPEL Document.
     * @return The E-BPEL Document.
     * @throws TransformerException If the transformation fails.
     */
    private Document transform(Document document) throws TransformerException {
        try {
            String systemId = document.getBaseURI().toString();

            String[] sheets = {  
                    "globalscope.xsl",
                    "elseif.xsl",
                    "repeatuntil.xsl", 
                    "variables.xsl", 
                    "documentation.xsl",
                    "extensions.xsl", 
                    "language.xsl",
                    "irra.xsl", 
                    "handlers.xsl",
                    "jointransition.xsl",
                    "attributes.xsl",
                    "defaults.xsl"
            };
            Transformer transformer;
            DOMSource source = new DOMSource(document, systemId);
            DOMResult result = null;

            for (int index = 0; index < sheets.length; index++) {

                System.out.println("Applying transformation 'xslt" + File.separator + sheets[index] + "'.");
                transformer = getTransformer(sheets[index]);
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
     * Command-line tool kit for transformation of WS-BPEL into E-BPEL.
     * <p>
     * Usage: Transform bpel [target]
     * <ul>
     * <li> bpel - the WS-BPEL source file</li>
     * <li>target (optional) - the E-BPEL target file</li>
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
     * 
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

}
