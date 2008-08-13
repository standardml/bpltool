package com.beepell.expression;

import java.io.File;

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.w3c.dom.Node;

import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * Test Property Alias Query
 *
 */
public class TestPropertyAliasQuery extends TestCase {
    
    /**
     * Test Property Alias Query
     *
     */
    public final void testExpressions() {
        try {
            ExpressionContext context = new TestExpressionContext();
            NamespaceContext nsc = new DocumentNamespaceContext(new File("test/com/beepell/expression/document.xml"));
            PropertyAliasQuery query;
            Node node;

            /*
             * Any legal XPath expression may be used. An absolute or relative
             * path can be used in a <vprop:propertyAlias> as both resolve to
             * the context node which is the root node. [8.2.6]
             * 
             * See [8.2.6] for examples of absolute and relative queries.
             */
 
            QName person = context.getVariableType("person");
            
            // Relative query
            query = new PropertyAliasQuery(person, "dmy:dateborn", nsc);
            node = query.evaluate("person", context);
            assertNotNull(node);
            assertEquals("1921-01-09", node.getTextContent());

            // Absolute query
            query = new PropertyAliasQuery(person, "/dmy:person/dmy:dateborn", nsc);
            node = query.evaluate("person", context);
            assertNotNull(node);
            assertEquals("1921-01-09", node.getTextContent());
            
            QName message = context.getVariableType("message");
            // Relative query, message type variable
            query = new PropertyAliasQuery(message, "person", "dmy:dateborn", nsc);
            node = query.evaluate("message", context);
            assertNotNull(node);
            assertEquals("1921-01-09", node.getTextContent());

            // Absolute query, message type variable
            query = new PropertyAliasQuery(message, "person", "/dmy:person/dmy:dateborn", nsc);
            node = query.evaluate("message", context);
            assertNotNull(node);
            assertEquals("1921-01-09", node.getTextContent());
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception.");
        }
    }
}
