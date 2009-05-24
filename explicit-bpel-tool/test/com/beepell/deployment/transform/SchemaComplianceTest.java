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

    protected void setUp() throws Exception {
        File file = new File("schemas/xslt20.xsd");
        SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        Schema schema = factory.newSchema(file);
        this.validator = schema.newValidator();

        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        this.builder = dbf.newDocumentBuilder();

    }

    public final void testBPELSchema() throws Exception {
        File file = new File("schemas/bpel.xsd");
        SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        Schema schema = factory.newSchema(file);
        this.validator = schema.newValidator();
    }

    public final void testEBPELSchema() throws Exception {
        File file = new File("schemas/e-bpel.xsd");
        SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        Schema schema = factory.newSchema(file);
        this.validator = schema.newValidator();
    }

    public final void testAttributes() throws Exception {
        Document document = builder.parse(new File("xslt/attributes.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testDefaults() throws Exception {
        Document document = builder.parse(new File("xslt/defaults.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testDocumentation() throws Exception {
        Document document = builder.parse(new File("xslt/documentation.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testElseIf() throws Exception {
        Document document = builder.parse(new File("xslt/elseif.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    
    public final void testExtensions() throws Exception {
        Document document = builder.parse(new File("xslt/extensions.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testGlobalScope() throws Exception {
        Document document = builder.parse(new File("xslt/globalscope.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testHandlers() throws Exception {
        Document document = builder.parse(new File("xslt/handlers.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testInvoke() throws Exception {
        Document document = builder.parse(new File("xslt/invoke.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testIRRA() throws Exception {
        Document document = builder.parse(new File("xslt/irra.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testJoinTransition() throws Exception {
        Document document = builder.parse(new File("xslt/jointransition.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testLanguage() throws Exception {
        Document document = builder.parse(new File("xslt/language.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testReceive() throws Exception {
        Document document = builder.parse(new File("xslt/receive.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testWhile() throws Exception {
        Document document = builder.parse(new File("xslt/while.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testReply() throws Exception {
        Document document = builder.parse(new File("xslt/reply.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testToFromParts() throws Exception {
        Document document = builder.parse(new File("xslt/tofromparts.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testVariables() throws Exception {
        Document document = builder.parse(new File("xslt/variables.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testSequece() throws Exception {
        Document document = builder.parse(new File("xslt/sequence.xsl"));
        this.validator.validate(new DOMSource(document));
    }

}
