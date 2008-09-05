package com.beepell.repository;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.net.URI;
import java.util.Collection;
import java.util.Iterator;

import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.serialize.XMLSerializer;
import com.sun.xml.xsom.XSComplexType;
import com.sun.xml.xsom.XSElementDecl;
import com.sun.xml.xsom.XSSchema;
import com.sun.xml.xsom.XSSchemaSet;
import com.sun.xml.xsom.XSSimpleType;
import com.sun.xml.xsom.XSType;
import com.sun.xml.xsom.parser.XSOMParser;

/**
 * A repository for receiving and containing definitions written in the XML
 * Schema language. This is implemented using the XML Schema Object Model, XSOM.
 * Note that, process definitions does not share repositories. Different process
 * definitions may use different versions of definitions in the namespace.
 * 
 * @author Tim Hallwyl
 */
public class SchemaRepository {

    private final XSOMParser parser;

    private XSSchemaSet schemaSet;

    private SchemaPool pool;

    /**
     * Create a SchemaRepository.
     * 
     * @throws ParserConfigurationException
     */
    public SchemaRepository() throws ParserConfigurationException {
        this.pool = new SchemaPool();
        this.parser = new XSOMParser();
        // parser.setErrorHandler(...);
        // parser.setEntityResolver(...);
        this.parser.setErrorHandler(new ErrorHandler());
    }

    /**
     * Adds an XML Schema document to the repository.
     * 
     * @param uri
     * @throws SAXException
     */
    public void add(URI uri) throws SAXException {
        // TODO: Test if it resolve relative uris in schema imports correctly.
        InputSource source = new InputSource(uri.toString());
        this.parser.parse(source);

        this.pool.add(uri);
    }

    /**
     * Add XML Schema definitions from a WSDL document.
     * 
     * @param uri URI to the WSDL document.
     */
    public void addWSDL(URI uri) {
        if (this.schemaSet != null)
            throw new IllegalStateException("Repository has been initialized");

        if (uri == null)
            throw new IllegalArgumentException("Argument uri most not be null.");

        try {
            // 1. Transform WSDL into XML Schema
            TransformerFactory factory = TransformerFactory.newInstance();
            URI suri = SchemaRepository.class.getResource("wsdl-schema.xsl").toURI();
            File file = new File(suri);
            StreamSource xsl = new StreamSource(new FileInputStream(file), suri.getPath());
            Transformer transformer = factory.newTransformer(xsl);

            Source source = new StreamSource(new File(uri));
            DOMResult result = new DOMResult();
            transformer.transform(source, result);
            Document schema = (Document) result.getNode();

            if (schema == null || schema.getDocumentElement() == null) {
                System.out.println("INFO: No schema was found in WSDL document at '" + uri + "'.");
                return;
            }

            // 2. Use serializer to parse the schema

            /*
             * For some reason, Piped Streams does not work with the
             * XMLSerializer. Well, the method is called setOutputByteStream but
             * the signature accept any OutputStream. As an alternative to use
             * ByteArrayOutputStream, we can use a temporary file, but it is to
             * messy.
             */

            /* WORKING CODE: USES BYTE ARRAY STREAMS */
            XMLSerializer serializer = new XMLSerializer();
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            serializer.setOutputByteStream(bos);
            serializer.serialize(schema.getDocumentElement());
            this.parser.parse(new InputSource(new ByteArrayInputStream(bos.toByteArray())));

            /* WORKING CODE: USES TEMP FILE */
            // File temp = new File("/tmp/schema.xml");
            // FileOutputStream out = new FileOutputStream(new
            // File("/tmp/schema.xml"));
            // XMLSerializer serializer = new XMLSerializer();
            // serializer.setOutputByteStream(out);
            // serializer.serialize(schema); parser.parse(temp);
            this.pool.add(new ByteArrayInputStream(bos.toByteArray()));

        } catch (Exception exception) {
            exception.printStackTrace();
        }

    }

    /**
     * This method initialize the XSOM schema set and Schema, if it is not
     * initialized already. When this is done no more schemas can be added. This
     * is method is called in all request methods.
     * 
     * @throws SAXException
     */
    private void initialize() throws SAXException {
        if (this.schemaSet == null)
            this.schemaSet = this.parser.getResult();
    }

    /**
     * Lookup an XML type (complex or simple).
     * 
     * @param qName qualified name of the type to find.
     * @return the type found or null if not found.
     * @throws SAXException
     */
    public XSType getType(QName qName) throws SAXException {
        if (qName == null)
            throw new IllegalArgumentException("SchemaRepository.getType: QName must not be null.");

        initialize();
        return this.schemaSet.getType(qName.getNamespaceURI(), qName.getLocalPart());
    }

    /**
     * Gets an XML Schema Complex Type by its Qualified Name.
     * 
     * @param qName qualified name of the type to find.
     * @return the type found.
     * @throws SAXException
     */
    public XSComplexType getComplexType(QName qName) throws SAXException {
        initialize();
        return this.schemaSet.getComplexType(qName.getNamespaceURI(), qName.getLocalPart());
    }

    /**
     * Gets an XML Schema Simple Type by its Qualified Name.
     * 
     * @param qName qualified name of the type to find.
     * @return the type found.
     * @throws SAXException
     */
    public XSSimpleType getSimpleType(QName qName) throws SAXException {
        initialize();
        return this.schemaSet.getSimpleType(qName.getNamespaceURI(), qName.getLocalPart());
    }

    /**
     * Gets an XML Schema Simple Type by its Qualified Name.
     * 
     * @param qName qualified name of the type to find.
     * @return the type found.
     * @throws SAXException
     * @throws SAXException
     */
    public XSElementDecl getElement(QName qName) throws SAXException {
        initialize();
        return this.schemaSet.getElementDecl(qName.getNamespaceURI(), qName.getLocalPart());
    }

    /**
     * Returns the Schema where the type is declared. Used for validation.
     * 
     * @param type
     * @return the Schema where the type is declared.
     * @throws SAXException
     */
    public Schema getSchema(QName type) throws SAXException {
        if (type == null)
            throw new NullPointerException("getSchema: QName type must not be null.");

        initialize();
        return this.pool.getSchema(type);
    }

    /* METHOD FOR DEBUGGING ONLY */

    /**
     * @throws SAXException
     */
    @SuppressWarnings("unused")
    private void status() throws SAXException {
        initialize();

        System.out.println("INFO: SchemaSize: " + this.schemaSet.getSchemaSize());
        System.out.println("INFO: Schemas: ");

        Collection<XSSchema> schemata = this.schemaSet.getSchemas();
        for (XSSchema schema : schemata) {
            System.out.println("INFO: - " + schema.getTargetNamespace());
        }

        Iterator<XSElementDecl> iterator = this.schemaSet.iterateElementDecls();
        XSElementDecl element;
        while (iterator.hasNext()) {
            element = iterator.next();
            System.out.println("INFO:  " + element.getName());
        }

        element = this.schemaSet.getElementDecl("http://beepell.com/samples/dummy/schema", "person");
        if (element != null)
            System.out.println("INFO: Found element {" + element.getTargetNamespace() + "}" + element.getName());
        else
            System.out.println("INFO: Element not found");

    }
}
