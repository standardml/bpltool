package com.beepell.execution.bpel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.io.File;

import javax.xml.xpath.XPathExpressionException;

import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.beepell.BPELConstants;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;

/**
 * @author Tim Hallwyl
 *
 */
public class EndpointAccessTest extends AbstractContextTest {

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        SchemaRepository schemas = new SchemaRepository();
        ServiceRepository services = new ServiceRepository();

        load(new File(EndpointAccessTest.class.getResource("endpointAccessTest.bpi").toURI()));    
        Document document = this.getInstance().getOwnerDocument();
        document.setUserData("com.beepell.repository.SchemaRepository", schemas, null);
        document.setUserData("com.beepell.repository.ServiceRepository", services, null);
    }
    
    /**
     * Test method for {@link com.beepell.execution.bpel.Context#getEndpoint(java.lang.String, Role)}.
     */
    @Test
    public final void testGetEndpoint() {
        try {
            Node node, endpoint;
            Context context;
            
            node = evaluate("//bpi:empty[@name='A']", this.getInstance());
            context = new Context(node);
            
            endpoint = context.getEndpoint("echoPartnerLink", Role.MY);            
            assertNotNull(endpoint);
            assertEquals("port", endpoint.getLocalName());
            assertEquals(BPELConstants.WSDL, endpoint.getNamespaceURI());
            assertNotNull(evaluate("soap:address", endpoint));
            assertNotNull(evaluate("soap:address/@location", endpoint));
            assertEquals("http://localhost:8080/test/echo", evaluate("soap:address/@location", endpoint).getNodeValue());
            
            assertNull(context.getEndpoint("echoPartnerLink", Role.PARTNER));
                        
            assertNull(context.getEndpoint("insurancePartnerLink", Role.MY));
            assertNull(context.getEndpoint("insurancePartnerLink", Role.PARTNER));
            
            node = evaluate("//bpi:empty[@name='B']", this.getInstance());
            context = new Context(node);
            
            endpoint = context.getEndpoint("echoPartnerLink", Role.MY);            
            assertNotNull(endpoint);
            assertEquals("port", endpoint.getLocalName());
            assertEquals(BPELConstants.WSDL, endpoint.getNamespaceURI());
            assertEquals("http://localhost:8080/test/echo", evaluate("soap:address/@location", endpoint).getNodeValue());
            
            assertNull(context.getEndpoint("echoPartnerLink", Role.PARTNER));
            
            endpoint = context.getEndpoint("insurancePartnerLink", Role.PARTNER);            
            assertNotNull(endpoint);
            assertEquals("port", endpoint.getLocalName());
            assertEquals(BPELConstants.WSDL, endpoint.getNamespaceURI());
            assertEquals("http://remotehost:8080/test/insurance", evaluate("soap:address/@location", endpoint).getNodeValue());
            
            assertNull(context.getEndpoint("insurancePartnerLink", Role.MY));
            
        } catch (XPathExpressionException exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }

}
