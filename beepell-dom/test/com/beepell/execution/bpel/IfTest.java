package com.beepell.execution.bpel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.io.File;

import javax.xml.xpath.XPathConstants;

import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;

/**
 * @author Tim Hallwyl
 *
 */
public class IfTest extends AbstractContextTest {


    /**
     * condition -> true, skip else-clause
     */
    @Test
    public final void testRewriteIf1() {
        try {
            SchemaRepository schemas = new SchemaRepository();
            ServiceRepository services = new ServiceRepository();

            load(new File(EndpointAccessTest.class.getResource("if1Test.bpi").toURI()));    
            Document document = this.getInstance().getOwnerDocument();
            document.setUserData("com.beepell.repository.SchemaRepository", schemas, null);
            document.setUserData("com.beepell.repository.ServiceRepository", services, null);
            
            Element ifActivity = (Element) evaluate("//bpi:if", this.getInstance());
            assertNotNull(ifActivity);
            
            Context context = new Context(ifActivity);
            Semantics.rewrite(ifActivity, context);
            
            assertNull(evaluate("//bpi:if", this.getInstance()));
            assertEquals("a1b3", evaluate("/bpi:instance/bpi:flow/bpi:sequence[1]/bpi:sources/bpi:source[1]/@linkName", this.getInstance(), XPathConstants.STRING));
            assertEquals("a1b2", evaluate("/bpi:instance/bpi:flow/bpi:sequence[1]/bpi:sources/bpi:source[2]/@linkName", this.getInstance(), XPathConstants.STRING));
            assertEquals("then", evaluate("/bpi:instance/bpi:flow/bpi:sequence[1]/bpi:empty/@name", this.getInstance(), XPathConstants.STRING));
            
            
        } catch (Exception exception) {
            fail("Unexpected exception: " + exception.getLocalizedMessage());
        }
        
    }

    /**
     * condition -> false, selected else-clause
     */
    @Test
    public final void testRewriteIf2() {
        try {
            SchemaRepository schemas = new SchemaRepository();
            ServiceRepository services = new ServiceRepository();

            load(new File(EndpointAccessTest.class.getResource("if2Test.bpi").toURI()));    
            Document document = this.getInstance().getOwnerDocument();
            document.setUserData("com.beepell.repository.SchemaRepository", schemas, null);
            document.setUserData("com.beepell.repository.ServiceRepository", services, null);
            
            Element ifActivity = (Element) evaluate("//bpi:if", this.getInstance());
            assertNotNull(ifActivity);
            
            Context context = new Context(ifActivity);
            Semantics.rewrite(ifActivity, context);
            
            assertNull(evaluate("//bpi:if", this.getInstance()));
            assertEquals("a1b3", evaluate("/bpi:instance/bpi:flow/bpi:sequence[1]/bpi:sources/bpi:source[1]/@linkName", this.getInstance(), XPathConstants.STRING));
            assertEquals("a1b2", evaluate("/bpi:instance/bpi:flow/bpi:sequence[1]/bpi:sources/bpi:source[2]/@linkName", this.getInstance(), XPathConstants.STRING));
            assertEquals("else", evaluate("/bpi:instance/bpi:flow/bpi:sequence[1]/bpi:empty/@name", this.getInstance(), XPathConstants.STRING));
            
            
        } catch (Exception exception) {
            fail("Unexpected exception: " + exception.getLocalizedMessage());
        }
        
    }

    /**
     * condition -> false, no else-clause
     */
    @Test
    public final void testRewriteIf3() {
        try {
            SchemaRepository schemas = new SchemaRepository();
            ServiceRepository services = new ServiceRepository();

            load(new File(EndpointAccessTest.class.getResource("if3Test.bpi").toURI()));    
            Document document = this.getInstance().getOwnerDocument();
            document.setUserData("com.beepell.repository.SchemaRepository", schemas, null);
            document.setUserData("com.beepell.repository.ServiceRepository", services, null);
            
            Element ifActivity = (Element) evaluate("//bpi:if", this.getInstance());
            assertNotNull(ifActivity);
            
            Context context = new Context(ifActivity);
            Semantics.rewrite(ifActivity, context);
            System.out.println(toString(this.getInstance()));
            
            assertNull(evaluate("//bpi:if", this.getInstance()));
            assertNull(evaluate("//bpi:sequence[2]", this.getInstance()));
            assertEquals("true", evaluate("//bpi:link[@name='a1b3']/@state", this.getInstance(), XPathConstants.STRING));
            assertEquals("false", evaluate("//bpi:link[@name='a1b2']/@state", this.getInstance(), XPathConstants.STRING));

            
            
            
        } catch (Exception exception) {
            fail("Unexpected exception: " + exception.getLocalizedMessage());
        }
        
    }
    
}
