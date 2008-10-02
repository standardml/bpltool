package com.beepell.execution.bpel;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.io.File;

import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;

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
        
        load(new File(VariableAccessTest.class.getResource("removeIncomingLinksTest.bpi").toURI()));    
        Document document = this.getInstance().getOwnerDocument();
        document.setUserData("com.beepell.repository.SchemaRepository", schemas, null);
        document.setUserData("com.beepell.repository.ServiceRepository", services, null);
    }
    
    /**
     * Test method for {@link com.beepell.execution.bpel.Context#removeIncomingLinks()}.
     */
    @Test
    public final void testRemoveIncomingLinks() {
        try {
            Node node;
            Context context;
            
            assertNotNull(this.getInstance());
            
            node = evaluate("//bpi:*[@name='a2']", this.getInstance());
            assertNotNull(node);
            
            // Make sure every thing is there
            assertNotNull(evaluate("bpi:targets", node));
            assertNotNull(evaluate("//bpi:flow[@name='main']/bpi:links/bpi:link[@name='ab']", this.getInstance()));
            assertNotNull(evaluate("//bpi:flow[@name='sub']/bpi:links/bpi:link[@name='ab']", this.getInstance()));
            assertNotNull(node);
            context = new Context(node);
            context.removeIncomingLinks();
            
            assertNull(evaluate("targets", node));
            
            // The main flow link ab must still be there
            assertNotNull(evaluate("//bpi:flow[@name='main']/bpi:links/bpi:link[@name='ab']", this.getInstance()));
            
            // The sub flow link ab must be removed
            assertNull(evaluate("//bpi:flow[@name='sun']/bpi:links/bpi:link[@name='ab']", this.getInstance()));
            
            // Targets are no loger there
            assertNull(evaluate("//bpi:*[@name='a2']/bpi:targets", this.getInstance()));
            assertNull(evaluate("bpi:targets", node));
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }
}
