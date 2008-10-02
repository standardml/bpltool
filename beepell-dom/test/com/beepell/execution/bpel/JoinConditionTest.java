package com.beepell.execution.bpel;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
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
public class JoinConditionTest extends AbstractContextTest {

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        
        SchemaRepository schemas = new SchemaRepository();
        
        ServiceRepository services = new ServiceRepository();
        
        load(new File(VariableAccessTest.class.getResource("joinConditionTest.bpi").toURI()));    
        Document document = this.getInstance().getOwnerDocument();
        document.setUserData("com.beepell.repository.SchemaRepository", schemas, null);
        document.setUserData("com.beepell.repository.ServiceRepository", services, null);
    
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
            
            node = evaluate("//bpi:empty[@name='A']", this.getInstance());
            assertNotNull(node);
            context = new Context(node);
            value = context.evaluateJoinCondition();
            assertTrue(value);
            
            node = evaluate("//bpi:empty[@name='B']", this.getInstance());
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
