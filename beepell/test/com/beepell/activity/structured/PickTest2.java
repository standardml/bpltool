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

import com.beepell.Settings;
import com.beepell.deployment.DeploymentManager;
import com.beepell.deployment.ProcessContext;
import com.beepell.util.XML;

import junit.framework.TestCase;

/**
 * Test of the Pick w/ createInstance="no".
 * @author Tim Hallwyl
 *
 */
public class PickTest2 extends TestCase {

    protected void setUp() throws Exception {
        super.setUp();
        Settings settings = Settings.getInstance();
        settings.setSetting("tools.tracker.enabled", "false");
        settings.setSetting("tools.tracker.mode", "run");
    }

    
    /**
     * Testing onAlarm
     *
     */
    public void testPickOnAlarm() {
        ProcessContext process = null;
        String result = "FAILED";
        try {
            File bpel = new File("test/com/beepell/activity/structured/pick2.bpel");
            process = DeploymentManager.deploy(bpel);
            
            System.out.println("Launch: Process has been deployed.");
            
            InvokeTextThread thread = new InvokeTextThread();
            thread.start();
            Thread.sleep(3000);
            result = thread.response;
            assertEquals("timeout", result);
            
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

    /**
     * Testin onMessage
     *
     */
    public void testPickOnMessage() {
        ProcessContext process = null;
        String result = "FAILED";
        try {
            File bpel = new File("test/com/beepell/activity/structured/pick2.bpel");
            process = DeploymentManager.deploy(bpel);
            
            System.out.println("Launch: Process has been deployed.");
            
            InvokeTextThread thread = new InvokeTextThread();
            thread.start();
            Thread.sleep(1000);
            
            int number = invokeNumber(42);
            assertEquals(84, number);
            Thread.sleep(100);
            
            result = thread.response;
            assertEquals("Hello world!", result);
            
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
    
    class InvokeTextThread extends Thread {
        /** response message text part */
        public String response;
        public void run() {
            try {
                response = PickTest2.invokeText("Hello world!");           
            } catch (Exception exception) {
                exception.printStackTrace();
                fail();
            }
        }
    }
    
    
    private static String invokeText(String text) throws SOAPException, IOException {
        QName service = new QName("http://beepell.com/samples/proxy/definitions", "textService");
        QName port = new QName("http://beepell.com/samples/proxy/definitions", "textPort");
        String endpointAddress = "http://localhost:9020/test/text";
        String soapAction = "http://beepell.com/test/text";

        MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
        SOAPMessage requestMessage = factory.createMessage();
        SOAPPart soap = requestMessage.getSOAPPart();
        SOAPEnvelope envelope = soap.getEnvelope();
        SOAPBody body = envelope.getBody();
        SOAPElement content = body.addBodyElement(envelope.createName("textOperation", "rpc", "http://beepell.com/samples/proxy/definitions"));
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
    
    
    private static int invokeNumber(int number) throws SOAPException, IOException {
        QName service = new QName("http://beepell.com/samples/proxy/definitions", "numberService");
        QName port = new QName("http://beepell.com/samples/proxy/definitions", "numberPort");
        String endpointAddress = "http://localhost:9030/test/number";
        String soapAction = "http://beepell.com/test/number";

        MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
        SOAPMessage requestMessage = factory.createMessage();
        SOAPPart soap = requestMessage.getSOAPPart();
        SOAPEnvelope envelope = soap.getEnvelope();
        SOAPBody body = envelope.getBody();
        SOAPElement content = body.addBodyElement(envelope.createName("numberOperation", "rpc", "http://beepell.com/samples/proxy/definitions"));
        SOAPElement partAccessorElement = content.addChildElement("number");
        partAccessorElement.setTextContent(String.valueOf(number));
        
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
        
        return Integer.parseInt(responseMessage.getSOAPBody().getTextContent());
        
    }
    
    
    protected void tearDown() throws Exception {
        super.tearDown();
    }

}
