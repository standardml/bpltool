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
public class JoinConditionTest extends AbstractContextTest {

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        
        SchemaRepository schemas = new SchemaRepository();
        
        ServiceRepository services = new ServiceRepository();
        
        File file = new File(VariableAccessTest.class.getResource("joinConditionTest.bpi").toURI());    
        this.instance = load(file);
        Document document = this.instance.getOwnerDocument();
        document.setUserData("com.beepell.repository.SchemaRepository", schemas, null);
        document.setUserData("com.beepell.repository.ServiceRepository", services, null);
    
        XPathFactory factory = XPathFactory.newInstance();
        this.xPath = factory.newXPath();
        this.xPath.setNamespaceContext(new DocumentNamespaceContext(this.instance.getOwnerDocument()));
    }
    
    /**
     * Test method for {@link com.beepell.execution.bpel.Context#evaluateJoinCondition()}.
     */
    @Test
    public final void testEvaluateJoinCondition() {
        try {
            Node node;
            Context context;
            boolean value;
            
            node = evaluate("//bpi:empty[@name='A']", this.instance);
            assertNotNull(node);
            context = new Context(node);
            value = context.evaluateJoinCondition();
            assertTrue(value);
            
            node = evaluate("//bpi:empty[@name='B']", this.instance);
            assertNotNull(node);
            context = new Context(node);
            value = context.evaluateJoinCondition();
            assertFalse(value);
            
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }

}
