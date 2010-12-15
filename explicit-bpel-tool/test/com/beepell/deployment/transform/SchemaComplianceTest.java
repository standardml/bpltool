package com.beepell.deployment.transform;
import java.io.File;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.w3c.dom.Document;

import junit.framework.TestCase;

public class SchemaComplianceTest extends TestCase {

    private Validator validator;
    private DocumentBuilder builder;
    private Transform transform;

    protected void setUp() throws Exception {
        File file = new File("schemas/xslt20.xsd");
        SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        Schema schema = factory.newSchema(file);
        this.validator = schema.newValidator();

        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        this.builder = dbf.newDocumentBuilder();

        this.transform = new Transform();
    }

    public final void testBPELSchema() throws Exception {
        File file = new File("schemas/bpel.xsd");
        SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        Schema schema = factory.newSchema(file);
        this.validator = schema.newValidator();
    }

    public final void testCBPELSchema() throws Exception {
        File file = new File("schemas/core-bpel.xsd");
        SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        Schema schema = factory.newSchema(file);
        this.validator = schema.newValidator();
    }
    
    public final void testStylesheets() throws Exception {
        for (String style_sheet : transform.getSheets()) {
            Document document = builder.parse(new File("xslt" + File.separator + style_sheet));
            this.validator.validate(new DOMSource(document));
        }
    }

}
