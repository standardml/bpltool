package com.beepell.activity.basic;

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

import junit.framework.TestCase;

import com.beepell.Settings;
import com.beepell.deployment.DeploymentManager;
import com.beepell.deployment.ProcessContext;
import com.beepell.util.XML;

/**
 * 
 * @author Tim Hallwyl
 *
 */
@WebServiceProvider
@ServiceMode(value = Service.Mode.MESSAGE)
public class InvokeTest extends TestCase implements Provider<SOAPMessage> {

    private Endpoint endpoint;
    protected void setUp() throws Exception {
        super.setUp();
        String endpointAddress = "http://localhost:9010/samples/services/Insurance";
        endpoint = Endpoint.create(this);
        endpoint.publish(endpointAddress);
        Settings settings = Settings.getInstance();
        settings.setSetting("tools.tracker.enabled", "false");
        settings.setSetting("tools.tracker.mode", "run");
    }
    
    protected void tearDown() throws Exception {
        super.tearDown();
        endpoint.stop();
    }
    
    
    /**
     * 
     *
     */
    public void testInvoke() {
        ProcessContext process = null;
        String result = "FAILED";
        try {
            File bpel = new File("test/com/beepell/activity/basic/invoke.bpel");
            process = DeploymentManager.deploy(bpel);
            System.out.println("Launch: Process has been deployed.");
            
            result = invoke();
            Thread.sleep(100); // Let process thread finish
            assertEquals("Monthly prime is 546.00 EUR, with a 200.00 EUR bonus pay-back for a damage free year", result);
        } catch (Exception exception) {
            exception.printStackTrace();
        } finally {
            System.out.print("Launch: Undeploying process...");
            if (process != null) 
                DeploymentManager.undeploy(process);
            System.out.println("done.");
        }
        
        if (result.equals("FAILED"))
            fail("Process FAILED");
    }

    
    private static String invoke() throws SOAPException, IOException {
        QName service = new QName("http://beepell.com/samples/proxy/definitions", "echoService");
        QName port = new QName("http://beepell.com/samples/proxy/definitions", "echoPort");
        String endpointAddress = "http://localhost:9040/samples/proxy/definitions/echo";
        String soapAction = "http://beepell.com/samples/proxy/definitions/echo";

        MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
        SOAPMessage requestMessage = factory.createMessage();
        SOAPPart soap = requestMessage.getSOAPPart();
        SOAPEnvelope envelope = soap.getEnvelope();
        SOAPBody body = envelope.getBody();
        SOAPElement content = body.addBodyElement(envelope.createName("echoOperation", "rpc", "http://beepell.com/samples/proxy/definitions"));
        SOAPElement partAccessorElement = content.addChildElement("text");
        partAccessorElement.setTextContent("Hello world!");
        
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

    public SOAPMessage invoke(SOAPMessage request) {
        try {
        String message = "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"><SOAP-ENV:Header/><SOAP-ENV:Body><rpc:InsuranceOperationResponse xmlns:rpc=\"http://tim.hallwyl.dk/response\"><premium xmlns:ins=\"http://tim.hallwyl.dk/insurance\"><ins:monthly>546.00</ins:monthly><ins:bonus>200.00</ins:bonus></premium></rpc:InsuranceOperationResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>";
        MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
        SOAPMessage response = factory.createMessage(null, new ByteArrayInputStream(message.getBytes()));
        return response;
        } catch (Exception exception) {
            fail();
            return null;
        }
    }
    
}
