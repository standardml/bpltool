package com.beepell.execution.bpel;

import static org.junit.Assert.*;

import java.io.File;

import javax.xml.xpath.XPathFactory;

import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.xml.namespace.DocumentNamespaceContext;

/**
 * @author Tim Hallwyl
 *
 */
public class RemoveIncomingLinksTest extends AbstractContextTest {

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        
        SchemaRepository schemas = new SchemaRepository();
        
        ServiceRepository services = new ServiceRepository();
        
        File file = new File(VariableAccessTest.class.getResource("removeIncomingLinksTest.bpi").toURI());    
        this.instance = load(file);
        Document document = this.instance.getOwnerDocument();
        document.setUserData("com.beepell.repository.SchemaRepository", schemas, null);
        document.setUserData("com.beepell.repository.ServiceRepository", services, null);
    
        XPathFactory factory = XPathFactory.newInstance();
        this.xPath = factory.newXPath();
        this.xPath.setNamespaceContext(new DocumentNamespaceContext(this.instance.getOwnerDocument()));
    }
    
    /**
     * Test method for {@link com.beepell.execution.bpel.Context#removeIncomingLinks()}.
     */
    @Test
    public final void testRemoveIncomingLinks() {
        try {
            Node node;
            Context context;
            
            assertNotNull(this.instance);
            
            node = evaluate("//bpi:*[@name='a2']", this.instance);
            assertNotNull(node);
            
            // Make sure every thing is there
            assertNotNull(evaluate("bpi:targets", node));
            assertNotNull(evaluate("//bpi:flow[@name='main']/bpi:links/bpi:link[@name='ab']", this.instance));
            assertNotNull(evaluate("//bpi:flow[@name='sub']/bpi:links/bpi:link[@name='ab']", this.instance));
            assertNotNull(node);
            context = new Context(node);
            context.removeIncomingLinks();
            
            assertNull(evaluate("targets", node));
            
            // The main flow link ab must still be there
            assertNotNull(evaluate("//bpi:flow[@name='main']/bpi:links/bpi:link[@name='ab']", this.instance));
            
            // The sub flow link ab must be removed
            assertNull(evaluate("//bpi:flow[@name='sun']/bpi:links/bpi:link[@name='ab']", this.instance));
            
            // Targets are no loger there
            assertNull(evaluate("//bpi:*[@name='a2']/bpi:targets", this.instance));
            assertNull(evaluate("bpi:targets", node));
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }
}
