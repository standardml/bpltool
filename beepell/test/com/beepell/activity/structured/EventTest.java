package com.beepell.activity.structured;

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
import javax.xml.ws.Service;
import javax.xml.ws.soap.SOAPBinding;

import junit.framework.TestCase;

import com.beepell.Settings;
import com.beepell.deployment.DeploymentManager;
import com.beepell.deployment.ProcessContext;
import com.beepell.util.XML;

/**
 * Test of the flow activity.
 * @author Tim Hallwyl
 *
 */
public class EventTest extends TestCase {

    private String result;
    
    /**
     * 
     * @param result
     */
    public void setResult(String result) {
        this.result = result;
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        Settings settings = Settings.getInstance();
        settings.setSetting("tools.tracker.enabled", "false");
        settings.setSetting("tools.tracker.mode", "run");
    }

    
    /**
     * Test of the flow activity.
     *
     */
    public void testFlow() {
        ProcessContext process = null;
        try {
            File bpel = new File("test/com/beepell/activity/structured/event.bpel");
            process = DeploymentManager.deploy(bpel);
            
            System.out.println("Launch: Process has been deployed.");
            
            Thread invoke = new Thread() {
                public void run() {
                  try { setResult(invoke("hello world")); }
                  catch (Exception exception) {
                      fail();
                  }
                }
            };
            
            invoke.start();
            Thread.sleep(2500);
            invokeEvent("okay");
            
            invoke.join();
            Thread.sleep(10000); // Let process thread finish
            assertEquals("okay", this.result);

            
        } catch (Exception exception) {
            exception.printStackTrace();
        } finally {
            System.out.print("Launch: Undeploying process...");
            if (process != null) 
                DeploymentManager.undeploy(process);
            System.out.println("done.");
        }

    }
    
    private static String invoke(String text) throws SOAPException, IOException {
        QName service = new QName("http://beepell.com/samples/proxy/definitions", "echoService");
        QName port = new QName("http://beepell.com/samples/proxy/definitions", "echoPort");
        String endpointAddress = "http://localhost:9060/samples/proxy/definitions/echo";
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
    
    private static String invokeEvent(String text) throws SOAPException, IOException {
        QName service = new QName("http://beepell.com/samples/proxy/definitions", "echoService");
        QName port = new QName("http://beepell.com/samples/proxy/definitions", "echoPort");
        String endpointAddress = "http://localhost:9060/samples/proxy/definitions/echo";
        String soapAction = "http://beepell.com/samples/proxy/definitions/event";

        MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
        SOAPMessage requestMessage = factory.createMessage();
        SOAPPart soap = requestMessage.getSOAPPart();
        SOAPEnvelope envelope = soap.getEnvelope();
        SOAPBody body = envelope.getBody();
        SOAPElement content = body.addBodyElement(envelope.createName("eventOperation", "rpc", "http://beepell.com/samples/proxy/definitions"));
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

    
    protected void tearDown() throws Exception {
        super.tearDown();
    }

}
