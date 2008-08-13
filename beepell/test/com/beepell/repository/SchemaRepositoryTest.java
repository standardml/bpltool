package com.beepell.repository;

import java.io.File;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URI;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.beepell.util.ErrorHandler;
import com.sun.xml.xsom.XSAttributeUse;
import com.sun.xml.xsom.XSComplexType;
import com.sun.xml.xsom.XSElementDecl;
import com.sun.xml.xsom.XSSimpleType;
import com.sun.xml.xsom.XSType;
import com.sun.xml.xsom.XSUnionSimpleType;

/**
 * Test of XML Schema Repository.
 * 
 * @author Tim Hallwyl
 */
public class SchemaRepositoryTest extends TestCase {

    private final String userDir = System.getProperty("user.dir");
    //private String base;
    
    @Override
    protected void setUp() throws Exception {
        super.setUp();
        System.setProperty("user.dir", new File("./test/com/beepell/repository/").getAbsolutePath());
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
        System.setProperty("user.dir", userDir);
    }



    /**
     * Add and retrieve complex type definitions.
     */
    public final void testComplexTypes() {

        try {
            SchemaRepository repository = new SchemaRepository();
            repository.add(new File("CoreComponentTypesDk.xsd").toURI());
            		
            XSComplexType type = repository.getComplexType(new QName("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", "AmountType"));

            assertTrue(type.isComplexType());
            assertFalse(type.isSimpleType());

            assertEquals("decimal simple type", type.getBaseType().toString());
            assertEquals("decimal simple type", type.getContentType().toString());

            assertTrue(type.isGlobal());
            assertFalse(type.isLocal());

            assertEquals("AmountType", type.getName());
            assertEquals("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", type.getTargetNamespace());

            XSAttributeUse attribute = type.getDeclaredAttributeUse("", "currencyID");
            assertTrue(attribute.isRequired());
            assertEquals("token simple type", attribute.getDecl().getType().toString());
            assertEquals("2002", type.getDeclaredAttributeUse("", "codeListVersionID").getDefaultValue().toString());

            assertEquals(2, type.getDeclaredAttributeUses().size());

        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }

    }

    /**
     * Add and retrieve simple type definitions.
     */
    public final void testSimpleType() {

        try {
            SchemaRepository repository = new SchemaRepository();
            
            repository.add(new File("PIECOMredefines.xsd").toURI());
            
            XSType xst = repository.getType(new QName("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", "UnionDateTimeType"));
            assertNotNull(xst);
            assertTrue(xst.isSimpleType());
            assertFalse(xst.isComplexType());
            
            XSSimpleType type = repository.getSimpleType(new QName("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", "UnionDateTimeType"));

            assertEquals("UnionDateTimeType", type.getName());
            assertEquals("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", type.getTargetNamespace());

            assertFalse(type.isComplexType());
            assertTrue(type.isSimpleType());
            assertTrue(type.isGlobal());
            assertFalse(type.isLocal());

            assertEquals("anySimpleType simple type", type.getBaseType().toString());
            assertTrue(type.isUnion());

            XSUnionSimpleType union = type.asUnion();
            assertEquals(2, union.getMemberSize());
            assertEquals("date", union.getMember(0).getName());
            assertEquals("dateTime", union.getMember(1).getName());

        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }
    }

    /**
     * Add and retrieve element definitions.
     */
    public final void testElement() {

        try {
            SchemaRepository repository = new SchemaRepository();
            repository.add(new File("pipStrict.xsd").toURI());
            
            XSElementDecl element = repository.getElement(new QName("http://rep.oio.dk/ubl/xml/schemas/0p71/pip/", "Invoice"));
            assertNotNull(element);

            assertEquals("Invoice", element.getName());
            assertEquals("http://rep.oio.dk/ubl/xml/schemas/0p71/pip/", element.getTargetNamespace());

            assertTrue(element.isGlobal());
            assertFalse(element.isLocal());

            assertEquals("InvoiceType complex type", element.getType().toString());

        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }
    }

    /**
     * Test access to schema definitions imported by added files, and indirectly
     * imported by imported files.
     */
    public final void testImportedSchemas() {
        try {
            SchemaRepository repository = new SchemaRepository();
            repository.add(new File("Invoice.xsd").toURI());
            
            XSComplexType complexType;
            // Type in directly imported schema
            complexType = repository.getComplexType(new QName("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", "BuyersReferenceIDType"));
            assertNotNull(complexType);
            assertEquals("BuyersReferenceIDType", complexType.getName());
            assertEquals("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", complexType.getTargetNamespace());

            // Type in indirectly imported schema (1. degree)
            complexType = repository.getComplexType(new QName("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", "AmountType"));
            assertNotNull(complexType);
            assertEquals("AmountType", complexType.getName());
            assertEquals("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", complexType.getTargetNamespace());

        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }
    }

    /**
     * Test adding multiple schemata and retrieving definitions.
     */
    public final void testMultipleSchemata() {
        try {
            SchemaRepository repository = new SchemaRepository();
            repository.add(new File("CoreComponentTypesDk.xsd").toURI());
            repository.add(new File("CoreComponentParametersDk.xsd").toURI());
            
            XSComplexType complexType;
            complexType = repository.getComplexType(new QName("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", "AmountType"));
            assertNotNull(complexType);
            assertEquals("AmountType", complexType.getName());
            assertEquals("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", complexType.getTargetNamespace());

            XSElementDecl element = repository.getElement(new QName("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", "ACC"));
            assertNotNull(element);
            assertEquals("ACC", element.getName());
            assertEquals("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", element.getTargetNamespace());

        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }

    }

    /**
     * Definitions within same namespace can be in several files.
     */
    public final void testMultipleSchemataSameNamespace() {
        // TODO: Implement test.
        // fail("Not implemented.");
    }

    /**
     * Tests adding a WSDL document including an XML Schema.
     */
    public final void testWSDLSchema() {
        try {
            URI wsdl = new File("correlation-services.wsdl").toURI();
            SchemaRepository repository = new SchemaRepository();
            repository.addWSDL(wsdl);

            String ns = "http://beepell.com/samples/correlation/schema";
            XSElementDecl element = repository.getElement(new QName(ns, "invoice"));
            assertNotNull(element);
            
            // ---------------------------
            
            URI wsdl2 = new File("../variable/service.wsdl").toURI();
            SchemaRepository repository2 = new SchemaRepository();
            repository2.addWSDL(wsdl2);
            String ns2 = "http://beepell.com/samples/dummy/schema";
            XSElementDecl element2 = repository2.getElement(new QName(ns2, "person"));
            assertNotNull(element2);
            

        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }
    }

    /**
     * getSchema(...) 
     */
    public final void testGetSchema() {
        try {
            
            SchemaRepository repository = new SchemaRepository();
            Schema defaultSchema = repository.getSchema(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "string"));
            String simple = "<myString xmlns:xsd=\""+ XMLConstants.W3C_XML_SCHEMA_NS_URI +"\" xmlns:xsi=\""+ XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI +"\" xsi:type=\"xsd:string\">Hellow World!</myString>";
            defaultSchema.newValidator().validate(new StreamSource(new StringReader(simple)));

            repository.add(new File("CoreComponentTypesDk.xsd").toURI());
            repository.add(new File("CoreComponentParametersDk.xsd").toURI());
            
            QName amount = new QName("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", "AmountType");
            XSComplexType complexType = repository.getComplexType(amount);
            assertNotNull(complexType);
            
            QName acc = new QName("http://rep.oio.dk/ubl/xml/schemas/0p71/common/", "ACC");
            XSElementDecl element = repository.getElement(acc);
            assertNotNull(element);

            
            String sampleAmount = "<myAmount xmlns:xsi=\""+ XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI +"\" xmlns:foo=\"http://rep.oio.dk/ubl/xml/schemas/0p71/common/\" xsi:type=\"foo:AmountType\" currencyID=\"DKK\">123.45</myAmount>";
            Schema schema = repository.getSchema(amount);
            Validator validator = schema.newValidator();
            validator.validate(new StreamSource(new StringReader(sampleAmount)));
            
            
            String sampleACC = "<foo:ACC xmlns:foo=\"http://rep.oio.dk/ubl/xml/schemas/0p71/common/\" foo:dictionaryEntryName=\"token\" foo:propertyTerm=\"token\" foo:representationTerm=\"token\" foo:definition=\"my sting text\"/>";
            
            Schema schema2 = repository.getSchema(acc);
            Validator validator2 = schema2.newValidator();
            validator2.setErrorHandler(new ErrorHandler());
            validator2.validate(new StreamSource(new StringReader(sampleACC)));
            
        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }
        
    }
    
    

    @SuppressWarnings("unused")
    private static boolean isValid(File xml, File schema) {
        try {
            Schema s = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema(schema);
            return isValid(new StreamSource(xml), s);
        } catch (Exception exception) {
            System.out.println("Document is not valid because ");
            System.out.println(exception.getMessage());
            return false;
        }
    }

    @SuppressWarnings("unused")
    private static boolean isValid(Document document, Schema schema) {
        return isValid(new DOMSource(document), schema);
    }

    private static boolean isValid(Source source, Schema schema) {
        try {
            Validator validator = schema.newValidator();
            validator.validate(source);
            return true;
        } catch (Exception exception) {
            System.out.println("Document is not valid because ");
            System.out.println(exception.getMessage());
            return false;
        }

    }

    /**
     * @param node
     * @return node tree as a string.
     */
    public static String nodeToString(Node node) {

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
