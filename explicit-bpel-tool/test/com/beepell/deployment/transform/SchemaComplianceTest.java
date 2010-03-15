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

    public final void testCBPELSchema() throws Exception {
        File file = new File("schemas/c-bpel.xsd");
        SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        Schema schema = factory.newSchema(file);
        this.validator = schema.newValidator();
    }

    public final void testAttributes() throws Exception {
        Document document = builder.parse(new File("xslt/attributes.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testDefaultConditions() throws Exception {
        Document document = builder.parse(new File("xslt/defaultconditions.xsl"));
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
   
    public final void testExtensionActivity() throws Exception {
        Document document = builder.parse(new File("xslt/extension-activity.xsl"));
        this.validator.validate(new DOMSource(document));
    }
    
    public final void testExtensionAssign() throws Exception {
        Document document = builder.parse(new File("xslt/extension-assign.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testExtensionAttributesElements() throws Exception {
        Document document = builder.parse(new File("xslt/extension-attributes-elements.xsl"));
        this.validator.validate(new DOMSource(document));
    }
    
    public final void testExtensionDeclarations() throws Exception {
        Document document = builder.parse(new File("xslt/extension-declarations.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testHandlers() throws Exception {
        Document document = builder.parse(new File("xslt/handlers.xsl"));
        this.validator.validate(new DOMSource(document));
    }
    
    public final void testIf() throws Exception {
        Document document = builder.parse(new File("xslt/if.xsl"));
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

    public final void testLanguage() throws Exception {
        Document document = builder.parse(new File("xslt/language.xsl"));
        this.validator.validate(new DOMSource(document));
    }
    
    public final void testPick() throws Exception {
        Document document = builder.parse(new File("xslt/pick.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testProcess() throws Exception {
        Document document = builder.parse(new File("xslt/process.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testReceive() throws Exception {
        Document document = builder.parse(new File("xslt/receive.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testReply() throws Exception {
        Document document = builder.parse(new File("xslt/reply.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testScope() throws Exception {
        Document document = builder.parse(new File("xslt/scope.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testSequece() throws Exception {
        Document document = builder.parse(new File("xslt/sequence.xsl"));
        this.validator.validate(new DOMSource(document));
    }
    
    public final void testToFromParts() throws Exception {
        Document document = builder.parse(new File("xslt/tofromparts.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    public final void testWhile() throws Exception {
        Document document = builder.parse(new File("xslt/while.xsl"));
        this.validator.validate(new DOMSource(document));
    }

    

}
