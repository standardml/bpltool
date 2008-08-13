package com.beepell.expression;

import java.io.File;

import javax.xml.namespace.NamespaceContext;

import junit.framework.TestCase;

import org.w3c.dom.Node;

import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * Test Copy Query.
 */
public class TestCopyQuery extends TestCase {

    /**
     * Test Copy Query.
     */
    public final void testExpressions() {
        try {
            ExpressionContext context = new TestExpressionContext();
            NamespaceContext nsc = new DocumentNamespaceContext(new File("test/com/beepell/expression/document.xml"));
            CopyQuery query;
            Node node;

            /*
             * Any legal XPath expression may be used. An absolute or relative
             * path can be used in a <vprop:propertyAlias> as both resolve to
             * the context node which is the root node. [8.2.6]
             * 
             * See [8.2.6] for examples of absolute and relative queries.
             */

            // Relative query
            query = new CopyQuery("dmy:dateborn", nsc);
            node = query.evaluate("person", context);
            assertNotNull(node);
            assertEquals("1921-01-09", node.getTextContent());

            // Absolute query
            query = new CopyQuery("/dmy:person/dmy:dateborn", nsc);
            node = query.evaluate("person", context);
            assertNotNull(node);
            assertEquals("1921-01-09", node.getTextContent());

            // There is no requirement that <query> return lvalues. [8.2.6]
            query = new CopyQuery("/dmy:person/dmy:age + 4", nsc);
            node = query.evaluate("person", context);
            assertNotNull(node);
            assertEquals("90", node.getTextContent());
            
            
            
            

        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception.");
        }
    }
}
