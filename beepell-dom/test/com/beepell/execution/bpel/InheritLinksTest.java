package com.beepell.execution.bpel;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.io.File;

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
public class InheritLinksTest extends AbstractContextTest {

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        
        load(new File(LinkStateTest.class.getResource("inheritLinksTest.bpi").toURI()));

        SchemaRepository schemas = new SchemaRepository();
        ServiceRepository services = new ServiceRepository();
        this.getInstance().getOwnerDocument().setUserData("com.beepell.repository.SchemaRepository", schemas, null);
        this.getInstance().getOwnerDocument().setUserData("com.beepell.repository.ServiceRepository", services, null);
    }

    /**
     * Test method for {@link com.beepell.execution.bpel.Context#inheritOutgoingLinks(org.w3c.dom.Element)}.
     */
    @Test
    public final void testInheritOutgoingLinks1() {
        try {
            assertNotNull(evaluate("//bpi:if[@name='a']/bpi:sources", this.getInstance()));
            assertNotNull(evaluate("//bpi:if[@name='a']/bpi:sources/bpi:source[@linkName='a1b2']", this.getInstance()));
            assertNotNull(evaluate("//bpi:if[@name='a']/bpi:sources/bpi:source[@linkName='a2b1']", this.getInstance()));
            assertNotNull(evaluate("//bpi:if[@name='a']/bpi:sources/bpi:source[@linkName='ab']", this.getInstance()));
            assertNull(evaluate("//bpi:empty[@name='b']/bpi:sources", this.getInstance()));

            
            Element contextNode = (Element) evaluate("//bpi:if[@name='a']", this.getInstance());
            assertNotNull(contextNode);
            Context context = new Context(contextNode);
            
            Element to = (Element) evaluate("//bpi:empty[@name='b']", this.getInstance());
            assertNotNull(to);
            
            context.inheritOutgoingLinks(to);
            assertNull(evaluate("//bpi:if[@name='a']/bpi:sources/bpi:source[@linkName='a1b2']", this.getInstance()));
            assertNull(evaluate("//bpi:if[@name='a']/bpi:sources/bpi:source[@linkName='a2b1']", this.getInstance()));
            assertNull(evaluate("//bpi:if[@name='a']/bpi:sources/bpi:source[@linkName='ab']", this.getInstance()));
            assertNull(evaluate("//bpi:if[@name='a']/bpi:sources", this.getInstance()));
            
            assertNotNull(evaluate("//bpi:empty[@name='b']/bpi:sources", this.getInstance()));
            assertNotNull(evaluate("//bpi:empty[@name='b']/bpi:sources/bpi:source[@linkName='a1b2']", this.getInstance()));
            assertNotNull(evaluate("//bpi:empty[@name='b']/bpi:sources/bpi:source[@linkName='a2b1']", this.getInstance()));
            assertNotNull(evaluate("//bpi:empty[@name='b']/bpi:sources/bpi:source[@linkName='ab']", this.getInstance()));
            
        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }
    
    /**
     * Test method for {@link com.beepell.execution.bpel.Context#inheritOutgoingLinks(org.w3c.dom.Element)}.
     */
    @Test
    public final void testInheritOutgoingLinks2() {
        try {
            assertNotNull(evaluate("//bpi:if[@name='c']/bpi:sources", this.getInstance()));
            assertNotNull(evaluate("//bpi:empty[@name='d']/bpi:sources/bpi:source[@linkName='a1b2']", this.getInstance()));
            assertNotNull(evaluate("//bpi:empty[@name='d']/bpi:sources/bpi:source[@linkName='a2b1']", this.getInstance()));
            assertNotNull(evaluate("//bpi:if[@name='c']/bpi:sources/bpi:source[@linkName='ab']", this.getInstance()));
            
            Element contextNode = (Element) evaluate("//bpi:if[@name='c']", this.getInstance());
            assertNotNull(contextNode);
            Context context = new Context(contextNode);
            
            Element to = (Element) evaluate("//bpi:empty[@name='d']", this.getInstance());
            assertNotNull(to);
            
            context.inheritOutgoingLinks(to);
            assertNull(evaluate("//bpi:if[@name='c']/bpi:sources/bpi:source[@linkName='a1b2']", this.getInstance()));
            assertNull(evaluate("//bpi:if[@name='c']/bpi:sources/bpi:source[@linkName='a2b1']", this.getInstance()));
            assertNull(evaluate("//bpi:if[@name='c']/bpi:sources/bpi:source[@linkName='ab']", this.getInstance()));
            assertNull(evaluate("//bpi:if[@name='c']/bpi:sources", this.getInstance()));
            
            assertNotNull(evaluate("//bpi:empty[@name='d']/bpi:sources", this.getInstance()));
            assertNotNull(evaluate("//bpi:empty[@name='d']/bpi:sources/bpi:source[@linkName='a1b2']", this.getInstance()));
            assertNotNull(evaluate("//bpi:empty[@name='d']/bpi:sources/bpi:source[@linkName='a2b1']", this.getInstance()));
            assertNotNull(evaluate("//bpi:empty[@name='d']/bpi:sources/bpi:source[@linkName='ab']", this.getInstance()));
            
        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }

}
