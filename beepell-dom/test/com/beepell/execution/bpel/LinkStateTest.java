package com.beepell.execution.bpel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;

import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;

import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Element;

import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;

/**
 * @author Tim Hallwyl
 * 
 */
public class LinkStateTest extends AbstractContextTest {

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        load(new File(LinkStateTest.class.getResource("linkStateTest.bpi").toURI()));

        SchemaRepository schemas = new SchemaRepository();
        ServiceRepository services = new ServiceRepository();
        this.getInstance().getOwnerDocument().setUserData("com.beepell.repository.SchemaRepository", schemas, null);
        this.getInstance().getOwnerDocument().setUserData("com.beepell.repository.ServiceRepository", services, null);
    }

    /**
     * Test method for
     * {@link com.beepell.execution.bpel.Context#getLinkState(java.lang.String)}.
     */
    @Test
    public final void testGetLinkState() {
        try {
            Element contextNode = (Element) evaluate("//bpi:scope[@name='A']/bpi:flow", this.getInstance());
            assertNotNull(contextNode);
            Context context = new Context(contextNode);

            Boolean state;
            // Test get unset value
            state = context.getLinkState("a1b2");
            assertNull(state);

            // Test get true value
            state = context.getLinkState("a2b1");
            assertTrue(state.booleanValue());

            // Test get false value
            state = context.getLinkState("ab");
            assertFalse(state.booleanValue());

            // Test get unknown link
            try {
                state = context.getLinkState("myMadeUpLinkName");
                fail("Exception expected.");
            } catch (Exception exception) {
                /* do nothing */
            }

            contextNode = (Element) evaluate("//bpi:sequence[@name='a1']/bpi:empty[2]", this.getInstance());
            assertNotNull(contextNode);
            context = new Context(contextNode);

            // Test lookup in immediately enclosed flow
            state = context.getLinkState("a1a2");
            assertNull(state);

            // Test shadowed lookup in outer flow
            state = context.getLinkState("ab");
            assertNull(state);

        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }

    }

    /**
     * Test method for
     * {@link com.beepell.execution.bpel.Context#setLinkState(java.lang.String, boolean)}.
     */
    @Test
    public final void testSetLinkState() {
        try {
            Element contextNode = (Element) evaluate("//bpi:sequence[@name='a1']/bpi:empty[2]", this.getInstance());
            assertNotNull(contextNode);
            Context context = new Context(contextNode);

            String state;
            
            // Test set true (local)
            context.setLinkState("a1a2", true);
            assertTrue(context.getLinkState("a1a2").booleanValue());
            state = (String) evaluate("//bpi:scope[@name='A']/bpi:flow/bpi:links/bpi:link[@name='a1a2']/@state", this.getInstance(), XPathConstants.STRING);
            assertEquals("true", state);
            
            // Test set false (outer)
            context.setLinkState("a1b2", false);
            state = (String) evaluate("/bpi:instance/bpi:flow/bpi:links/bpi:link[@name='a1b2']/@state", this.getInstance(), XPathConstants.STRING);
            assertEquals("false", state);

            
            // Test set shadowed link state
            context.setLinkState("ab", true);
            state = (String) evaluate("//bpi:scope[@name='A']/bpi:flow/bpi:links/bpi:link[@name='ab']/@state", this.getInstance(), XPathConstants.STRING);
            assertEquals("true", state);
            
            state = (String) evaluate("/bpi:instance/bpi:flow/bpi:links/bpi:link[@name='ab']/@state", this.getInstance(), XPathConstants.STRING);
            assertEquals("false", state);
            
            // Test set already determined link
            try {
                context.setLinkState("a1a2", false);
                fail("Exception expected.");
            } catch (Exception exception) {
                /* do nothing */
            }
            
            // Test set unknown link
            try {
                context.setLinkState("myMadeUpLinkName", false);
                fail("Exception expected.");
            } catch (Exception exception) {
                /* do nothing */
            }
            
        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }

    }

}
