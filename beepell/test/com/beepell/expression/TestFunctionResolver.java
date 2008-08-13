package com.beepell.expression;

import java.io.File;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

import com.beepell.BPELConstants;
import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * Testing FunctionResolver. NOTE: This is only a test of the FunctionResolver,
 * which is an adapter class. Thus, this is NOT a test of the
 * getVariableProperty implementation. However, it does test the XSLT
 * transformation directly, as it is implemented in the resolver adapter.
 * 
 * @author Tim Hallwyl
 */
public class TestFunctionResolver extends TestCase {

    private String dmy = "http://beepell.com/samples/dummy/schema";

    /**
     * Testing bpel:getVariableProperty adapter methods.
     */
    public final void testGetVariableProperty() {

        try {
            DocumentNamespaceContext nsc = new DocumentNamespaceContext(new File("test/com/beepell/expression/document.xml"));
            nsc.add("bpel", BPELConstants.BPEL);
            String expression;
            ExpressionContext context = new TestExpressionContext();
            FunctionResolver resolver = new FunctionResolver(context, nsc);

            /*
             * http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6211561 As a
             * workaround to bug 6211561, we need an empty document to use as
             * context node, when no context node is needed.
             */
            DocumentBuilderFactory dFactory = DocumentBuilderFactory.newInstance();
            dFactory.setValidating(false);
            dFactory.setNamespaceAware(true);
            DocumentBuilder dBuilder = dFactory.newDocumentBuilder();
            Document document = dBuilder.newDocument();

            XPathFactory factory = XPathFactory.newInstance();
            factory.setXPathFunctionResolver(resolver);
            XPath xpath = factory.newXPath();
            xpath.setNamespaceContext(nsc);

            // SIMPLE TYPES -- assume implicit conversion
            expression = "bpel:getVariableProperty('myProperty', 'dmy:tBln')";
            assertEquals(true, xpath.evaluate(expression, document, XPathConstants.BOOLEAN));

            // -- this will fail if implicit conversion is not applied.
            expression = "bpel:getVariableProperty('myProperty', 'dmy:fBln')";
            assertEquals(false, xpath.evaluate(expression, document, XPathConstants.BOOLEAN));

            // -- this will fail if implicit conversion is not applied.
            expression = "bpel:getVariableProperty('myProperty', 'dmy:flt')";
            assertEquals(1234, Math.round(1000 * ((Double) xpath.evaluate(expression, document, XPathConstants.NUMBER))));

            // -- this will fail if implicit conversion is not applied.
            expression = "bpel:getVariableProperty('myProperty', 'dmy:ntgr')";
            assertEquals(new Double(-42), xpath.evaluate(expression, document, XPathConstants.NUMBER));

            // -- this will fail if implicit conversion is not applied.
            expression = "bpel:getVariableProperty('myProperty', 'dmy:untgr')";
            assertEquals(new Double(42), xpath.evaluate(expression, document, XPathConstants.NUMBER));

            // COMPLEX TYPES
            expression = "bpel:getVariableProperty('myProperty', 'dmy:personinfo')";
            Element personinfo = (Element) xpath.evaluate(expression, document, XPathConstants.NODE);
            assertEquals("employee", personinfo.getLocalName());
            assertEquals("Simon", personinfo.getElementsByTagNameNS(dmy, "firstname").item(0).getTextContent());
            assertEquals("Spies", personinfo.getElementsByTagNameNS(dmy, "lastname").item(0).getTextContent());

            // ELEMENT
            expression = "bpel:getVariableProperty('myProperty', 'dmy:person')";
            Element person = (Element) xpath.evaluate(expression, document, XPathConstants.NODE);
            assertEquals("person", person.getLocalName());
            assertEquals("Simon", person.getElementsByTagNameNS(dmy, "firstname").item(0).getTextContent());
            assertEquals("Spies", person.getElementsByTagNameNS(dmy, "lastname").item(0).getTextContent());
            assertEquals("86", person.getElementsByTagNameNS(dmy, "age").item(0).getTextContent());
            assertEquals("1921-01-09", person.getElementsByTagNameNS(dmy, "dateborn").item(0).getTextContent());

        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception.");
        }
    }

    /**
     * Testing doXslTransform()
     *
     */
    public final void testDoXslTransform() {
        try {
            DocumentNamespaceContext nsc = new DocumentNamespaceContext(new File("test/com/beepell/expression/document.xml"));
            nsc.add("bpel", BPELConstants.BPEL);
            String expression;
            ExpressionContext context = new TestExpressionContext();
            FunctionResolver functions = new FunctionResolver(context, nsc);
            VariableResolver variables = new VariableResolver(context);

            /*
             * http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6211561 As a
             * workaround to bug 6211561, we need an empty document to use as
             * context node, when no context node is needed.
             */
            DocumentBuilderFactory dFactory = DocumentBuilderFactory.newInstance();
            dFactory.setValidating(false);
            dFactory.setNamespaceAware(true);
            DocumentBuilder dBuilder = dFactory.newDocumentBuilder();
            Document document = dBuilder.newDocument();

            XPathFactory factory = XPathFactory.newInstance();
            factory.setXPathFunctionResolver(functions);
            factory.setXPathVariableResolver(variables);
            XPath xpath = factory.newXPath();
            xpath.setNamespaceContext(nsc);
            
            String path = new File("./test/com/beepell/expression/xml-style.xsl").toURI().toString();
            expression = "bpel:doXslTransform('"+ path +"', $person/self::node(), 'myGlobalParameter', true())";
            Element employee = (Element) xpath.evaluate(expression, document, XPathConstants.NODE);
            assertEquals("employee", employee.getLocalName());
            assertEquals("Simon", employee.getElementsByTagNameNS(dmy, "firstname").item(0).getTextContent());
            assertEquals("Spies", employee.getElementsByTagNameNS(dmy, "lastname").item(0).getTextContent()); 
            
            expression = "bpel:doXslTransform('"+ path +"', $person/self::node(), 'myGlobalParameter', true())/dmy:firstname";
            Element firstname = (Element) xpath.evaluate(expression, document, XPathConstants.NODE);
            assertEquals("firstname", firstname.getLocalName());
            assertEquals("Simon", firstname.getTextContent());
            
            path = new File("./test/com/beepell/expression/text-style.xsl").toURI().toString();
            expression = "bpel:doXslTransform('"+ path +"', $person/self::node(), 'myGlobalParameter', true())";
            Text text = (Text) xpath.evaluate(expression, document, XPathConstants.NODE);
            assertEquals("Spies, Simon", text.getTextContent());
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception.");
        }

    }

}
