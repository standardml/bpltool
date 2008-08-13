package com.beepell.broker;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;

import javax.xml.namespace.QName;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPConstants;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPEnvelope;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Endpoint;
import javax.xml.ws.Provider;
import javax.xml.ws.Service;
import javax.xml.ws.ServiceMode;
import javax.xml.ws.WebServiceProvider;
import javax.xml.ws.soap.SOAPBinding;

import org.w3c.dom.NodeList;

import com.beepell.Settings;
import com.beepell.deployment.DeploymentManager;
import com.beepell.deployment.ProcessContext;
import com.beepell.util.XML;

import junit.framework.TestCase;

/**
 * @author Tim Hallwyl
 */
@WebServiceProvider
@ServiceMode(value = Service.Mode.MESSAGE)
public class InvokeFaultTest extends TestCase implements Provider<SOAPMessage> {

    private Endpoint endpoint;

    protected void setUp() throws Exception {
        super.setUp();
        String endpointAddress = "http://localhost:9047/samples/invokable/endpoint";
        endpoint = Endpoint.create(this);
        endpoint.publish(endpointAddress);
        Settings settings = Settings.getInstance();
        settings.setSetting("tools.tracker.enabled", "false");
        settings.setSetting("tools.tracker.mode", "step");
    }

    protected void tearDown() throws Exception {
        super.tearDown();
        endpoint.stop();
    }

    /**
     * 
     *
     */
    public void testInvokeFaultMessage() {
        ProcessContext process = null;
        
        try {
            File bpel = new File("test/com/beepell/broker/invokable.bpel");
            process = DeploymentManager.deploy(bpel);
            System.out.println("Launch: Process has been deployed.");

            assertEquals("fourth", invoke("4"));
            assertEquals("second", invoke("2"));
            assertEquals("third", invoke("3"));
            assertEquals("first", invoke("1"));
            
            Thread.sleep(3000);
            
            
        } catch (Exception exception) {
            exception.printStackTrace();
            // TODO: ?
        } finally {
            System.out.print("Launch: Undeploying process...");
            if (process != null)
                DeploymentManager.undeploy(process);
            System.out.println("done.");
        }
    }

    public SOAPMessage invoke(SOAPMessage request) {
        try {
            NodeList list = request.getSOAPBody().getElementsByTagNameNS("http://beepell.com/samples/invokable/definitions/faultOperation", "faultOperation");
            if (list.getLength() != 1)
                fail("Request message for faultOperation is invalid!");
            
            short number = Short.parseShort(list.item(0).getTextContent());
            
            String title = "Fault " + number + " occurred.";
            String detail = "A artificial fault with number " + number + " occurred in response to your request.";
            String letter;
            
            switch (number) {
            case 1:
                letter = "A";
                break;

            case 2:
                letter = "B";
                break;

            case 3:
                letter = "C";
                break;

            case 4:
                letter = "D";
                break;

            case 5:
                letter = "E";
                break;

            default:
                letter = "X";
                break;
            }
            
            String fault = "<soap:Fault xmlns:soap='http://schemas.xmlsoap.org/soap/envelope/' ><faultcode>soap:Server.ProcessingError</faultcode><faultstring>" + title + "</faultstring><detail><flt:fault"+ letter +" xmlns:flt='http://beepell.com/samples/invokable/schema'><flt:number>"+ number +"</flt:number><flt:title>"+ title +"</flt:title><flt:detail>"+ detail +"</flt:detail></flt:fault"+ letter +"></detail></soap:Fault>";
            String message = "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"><SOAP-ENV:Header/><SOAP-ENV:Body>"+ fault +"</SOAP-ENV:Body></SOAP-ENV:Envelope>";

            MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
            SOAPMessage response = factory.createMessage(null, new ByteArrayInputStream(message.getBytes()));
            
            return response;
            } catch (Exception exception) {
                fail();
                return null;
            }
    }
    
    private static String invoke(String text) throws SOAPException, IOException {
        QName service = new QName("http://beepell.com/samples/proxy/definitions", "echoService");
        QName port = new QName("http://beepell.com/samples/proxy/definitions", "echoPort");
        String endpointAddress = "http://localhost:9099/samples/proxy/definitions/echo";
        String soapAction = "http://beepell.com/samples/proxy/definitions/echo";

        MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
        SOAPMessage requestMessage = factory.createMessage();
        SOAPPart soap = requestMessage.getSOAPPart();
        SOAPEnvelope envelope = soap.getEnvelope();
        SOAPBody body = envelope.getBody();
        SOAPElement content = body.addBodyElement(envelope.createName("echoOperation", "rpc", "http://beepell.com/samples/proxy/definitions"));
        SOAPElement partAccessorElement = content.addChildElement("text");
        partAccessorElement.setTextContent(text);
        
        requestMessage.getMimeHeaders().addHeader("SOAPAction", "\"" + soapAction + "\"");
        System.out.println("---REQUEST START---");
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        requestMessage.writeTo(os);
        try { System.out.println(XML.toString(XML.toNode(os.toString()))); } catch (Exception e) { /* ignore */ }
        System.out.println("\n---REQUEST STOP---");
        
        Service webservice = Service.create(service);
        webservice.addPort(port, SOAPBinding.SOAP11HTTP_BINDING, endpointAddress);
        Dispatch<SOAPMessage> dispatch = webservice.createDispatch(port, SOAPMessage.class, Service.Mode.MESSAGE);
        SOAPMessage responseMessage = dispatch.invoke(requestMessage);
        
        System.out.println("---REFORMATTED RESPONSE START---");
        ByteArrayOutputStream os2 = new ByteArrayOutputStream();
        responseMessage.writeTo(os2);
        try { System.out.println(XML.toString(XML.toNode(os2.toString()))); } catch (Exception e) { /* ignore */ }
        System.out.println("\n---REFORMATTED RESPONSE STOP---");
        
        return responseMessage.getSOAPBody().getTextContent();
        
    }
}
