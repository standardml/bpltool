package com.beepell.repository;

import java.net.URI;
import java.util.List;

import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import com.beepell.BPELConstants;

import junit.framework.TestCase;

/**
 * Test for ServiceRepository.
 * 
 * @author Tim Hallwyl
 */
public class ServiceRepositoryTest extends TestCase {

    /**
     * Test adding a simple wsdl document with BPEL specifics.
     */
    public void testSimpleAccess() {
        try {
            ServiceRepository repository = new ServiceRepository();
            repository.add(new URI("./test/com/beepell/repository/echo-services.wsdl"));

            // Access and examine PartnerLinkType
            PartnerLinkType type = repository.getPartnerLinkType(new QName("http://beepell.com/samples/proxy/definitions", "echoPartnerLinkType"));
            assertNotNull(type);
            Role role = type.getRole("echoProviderRole");
            assertNotNull(role);
            PortType port = role.getPortType();
            assertNotNull(port);
            Operation operation = port.getOperation("echoOperation", null, null);
            assertNotNull(operation);
            Input input = operation.getInput();
            assertNotNull(input);
            Message inputMessage = input.getMessage();
            assertNotNull(inputMessage);
            Part part = inputMessage.getPart("text");
            assertNotNull(part);
            assertEquals(new QName(BPELConstants.XSD, "string"), part.getTypeName());
            Message outputMessage = operation.getOutput().getMessage();
            assertEquals(inputMessage, outputMessage);

            // Direct Access to Message
            Message message = repository.getMessage(new QName("http://beepell.com/samples/proxy/definitions", "echoMessage"));
            assertNotNull(message);
            assertEquals(inputMessage, message);

        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }
    }

    /**
     * Test access to variable property definitions.
     */
    public final void testProperties() {
        try {
            String nsuri = "http://beepell.com/samples/correlation/service";
            ServiceRepository repository = new ServiceRepository();
            repository.add(new URI("./test/com/beepell/repository/correlation-services.wsdl"));
            
            List<MessagePropertyAlias> aliases = repository.getPropertyAliasesByMessage(new QName(nsuri, "confirmMessage"));
            assertNotNull(aliases);
            assertEquals(1, aliases.size());
            MessagePropertyAlias alias =  (MessagePropertyAlias) aliases.get(0);
            assertEquals(new QName(nsuri, "orderId"), alias.getProperty().getName());
            assertEquals("sch:orderId", alias.getQuery());
            
            
            
        } catch (Exception e) {
            e.printStackTrace();
            fail("Caught unexpected exception");
        }
    }

}
