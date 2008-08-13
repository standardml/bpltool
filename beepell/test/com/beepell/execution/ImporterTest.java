package com.beepell.execution;

import java.io.ByteArrayOutputStream;
import java.io.File;

import javax.xml.namespace.QName;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPConstants;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPEnvelope;
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
 * @author Tim Hallwyl
 *
 */
@WebServiceProvider
@ServiceMode(value = Service.Mode.MESSAGE)
public class ImporterTest  extends TestCase implements Provider<SOAPMessage> {
    
    private SOAPMessage invoice = null;
    
    protected void setUp() throws Exception {
        super.setUp();
        Settings settings = Settings.getInstance();
        settings.setSetting("tools.tracker.enabled", "false");
        settings.setSetting("tools.tracker.mode", "run");
    }

    /**
     * 
     */
    public void testImporter() {
        String endpointAddress = "http://localhost:9021/samples/correlation/trade/test";
        Endpoint endpoint;
        endpoint = Endpoint.create(this);
        endpoint.publish(endpointAddress);
        
        ProcessContext process = null;
        this.invoice = null;
        
        try {
            File importer = new File("test/com/beepell/execution/importer.bpel");
            process = DeploymentManager.deploy(importer);           
            System.out.println("Launch: IMPORTER process has been deployed.");

            invokeImporterOrder(12);            
            invokeImporterOrder(21);            
            Thread.sleep(2000);
            invokeImporterInvoice(21);
            Thread.sleep(1000);
            invokeImporterInvoice(22);
            Thread.sleep(1000);
            invokeImporterInvoice(12);
            Thread.sleep(1000);
            
            assertNotNull(invoice);
            assertEquals("12", invoice.getSOAPBody().getElementsByTagNameNS("http://beepell.com/samples/correlation/trade/schema", "order").item(0).getTextContent());
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail();
        } finally {
            System.out.print("Launch: Undeploying process...");
            endpoint.stop();
            if (process != null) 
                DeploymentManager.undeploy(process);
            System.out.println("done.");
        }
        
    }    
  
    
    private static void invokeImporterOrder(int id) throws Exception {
        QName service = new QName("http://beepell.com/samples/correlation/trade", "producerService");
        QName port = new QName("http://beepell.com/samples/correlation/trade", "producerPort");
        String endpointAddress = "http://localhost:9120/samples/correlation/trade/importer/vendor";
        String soapAction = "http://beepell.com/samples/correlation/trade";

        MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
        SOAPMessage requestMessage = factory.createMessage();
        SOAPPart soap = requestMessage.getSOAPPart();
        SOAPEnvelope envelope = soap.getEnvelope();
        SOAPBody body = envelope.getBody();
        SOAPElement content = body.addBodyElement(envelope.createName("order", "rpc", "http://beepell.com/samples/correlation/trade"));
        SOAPElement partAccessorElement = content.addChildElement("order");
        SOAPElement order = partAccessorElement.addChildElement("id", "foo", "http://beepell.com/samples/correlation/trade/schema");
        order.setTextContent(String.valueOf(id));
        
        requestMessage.getMimeHeaders().addHeader("SOAPAction", "\"" + soapAction + "\"");
        System.out.println("---REQUEST START---");
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        requestMessage.writeTo(os);
        try { System.out.println(XML.toString(XML.toNode(os.toString()))); } catch (Exception e) { /* ignore */ }
        System.out.println("\n---REQUEST STOP---");
        
        Service webservice = Service.create(service);
        webservice.addPort(port, SOAPBinding.SOAP11HTTP_BINDING, endpointAddress);
        Dispatch<SOAPMessage> dispatch = webservice.createDispatch(port, SOAPMessage.class, Service.Mode.MESSAGE);
        dispatch.invokeOneWay(requestMessage);        
    }
    
    private static void invokeImporterInvoice(int id) throws Exception {
        QName service = new QName("http://beepell.com/samples/correlation/trade", "producerService");
        QName port = new QName("http://beepell.com/samples/correlation/trade", "producerPort");
        String endpointAddress = "http://localhost:9120/samples/correlation/trade/importer/vendee";
        String soapAction = "http://beepell.com/samples/correlation/trade";

        MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
        SOAPMessage requestMessage = factory.createMessage();
        SOAPPart soap = requestMessage.getSOAPPart();
        SOAPEnvelope envelope = soap.getEnvelope();
        SOAPBody body = envelope.getBody();
        SOAPElement content = body.addBodyElement(envelope.createName("invoice", "rpc", "http://beepell.com/samples/correlation/trade"));
        SOAPElement partAccessorElement = content.addChildElement("invoice");
        SOAPElement invoice = partAccessorElement.addChildElement("id", "foo", "http://beepell.com/samples/correlation/trade/schema");
        invoice.setTextContent(String.valueOf(id));
        SOAPElement order = partAccessorElement.addChildElement("order", "foo", "http://beepell.com/samples/correlation/trade/schema");
        order.setTextContent(String.valueOf(id));
        
        requestMessage.getMimeHeaders().addHeader("SOAPAction", "\"" + soapAction + "\"");
        System.out.println("---REQUEST START---");
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        requestMessage.writeTo(os);
        try { System.out.println(XML.toString(XML.toNode(os.toString()))); } catch (Exception e) { /* ignore */ }
        System.out.println("\n---REQUEST STOP---");
        
        Service webservice = Service.create(service);
        webservice.addPort(port, SOAPBinding.SOAP11HTTP_BINDING, endpointAddress);
        Dispatch<SOAPMessage> dispatch = webservice.createDispatch(port, SOAPMessage.class, Service.Mode.MESSAGE);
        dispatch.invokeOneWay(requestMessage);        
    }
    
    
    
    public SOAPMessage invoke(SOAPMessage request) {
        invoice = request;
        try { request.writeTo(System.out); } catch (Exception exception) { System.err.println("Failed to print message"); }
        return null;
    }
    
}
