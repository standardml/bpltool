package dk.itu.research.pls.bpl.bpel.samples.travelplanner;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.net.URL;

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

import com.beepell.Settings;
import com.beepell.deployment.DeploymentManager;
import com.beepell.deployment.ProcessContext;
import com.beepell.util.XML;

/**
 * A Java application to deploy and invoke a process.
 * 
 * @author Tim Hallwyl
 */
@WebServiceProvider
@ServiceMode(value = Service.Mode.MESSAGE)
public class Launch implements Provider<SOAPMessage> {

    private Launch(String fileName) {
        Endpoint endpoint;
        String endpointAddress = "http://localhost:8008/samples/booking";
        endpoint = Endpoint.create(this);
        endpoint.publish(endpointAddress);  
        
        Settings settings = Settings.getInstance();
        settings.setSetting("tools.tracker.enabled", "true");
        settings.setSetting("tools.tracker.mode", "step");

        ProcessContext process = null;
        try {
            if (fileName == null) {
                /* Run the exchange test case */
                //System.out.println(Launch.class.getResource("planner.wsdl").toString());
                //URL url = Launch.class.getResource("planner.bpel");

                File bpel = new File("./samples/dk/itu/research/pls/bpl/bpel/samples/travelplanner/planner.bpel");
                process = deploy(bpel);
                Thread.sleep(500);
                invoke();
                Thread.sleep(30000);
            } else {
                long time = Long.MAX_VALUE;
                File bpel = new File(fileName);
                process = deploy(bpel);
                Thread.sleep(time);
            }
        } catch (Exception exception) {
            exception.printStackTrace();
            System.exit(1);
        } finally {
            System.out.print("Launch: Undeploying process...");
            if (process != null)
                DeploymentManager.undeploy(process);
            System.out.println("done.");
        }
    }

    /**
     * Deploy a process or run an example.
     * <p>
     * Usage: Launch process.bpel
     * 
     * @param args
     */
    public static void main(String[] args) {

        if (args.length == 0)
            new Launch(null);
        else
            new Launch(args[0]);

    }

    private static ProcessContext deploy(File bpel) throws Exception {
        ProcessContext process = DeploymentManager.deploy(bpel);
        System.out.println("Launch: Process has been deployed.");
        return process;
    }

    private static void invoke() throws SOAPException, IOException {
        QName service = new QName("http://www.itu.dk/research/pls/bpl/bpel/samples/wsdl", "plannerService");
        QName port = new QName("http://www.itu.dk/research/pls/bpl/bpel/samples/wsdl", "plannerPort");
        String endpointAddress = "http://localhost:8080/samples/travelplanner";
        String soapAction = "http://www.itu.dk/research/pls/bpl/bpel/samples/wsdl/plan";

        MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
        SOAPMessage requestMessage = factory.createMessage();
        SOAPPart soap = requestMessage.getSOAPPart();
        SOAPEnvelope envelope = soap.getEnvelope();
        SOAPBody body = envelope.getBody();
        SOAPElement content = body.addBodyElement(envelope.createName("planOperation", "rpc", "http://www.itu.dk/research/pls/bpl/bpel/samples/wsdl"));

        SOAPElement beginsPartAccessorElement = content.addChildElement("begins");
        beginsPartAccessorElement.setTextContent("2009-09-24");

        SOAPElement endsPartAccessorElement = content.addChildElement("ends");
        endsPartAccessorElement.setTextContent("2009-10-05");

        SOAPElement flightPartAccessorElement = content.addChildElement("flight");
        flightPartAccessorElement.setTextContent("business");

        SOAPElement hotelPartAccessorElement = content.addChildElement("hotel");
        hotelPartAccessorElement.setTextContent("4");

        SOAPElement carPartAccessorElement = content.addChildElement("car");
        carPartAccessorElement.setTextContent("B");

        requestMessage.getMimeHeaders().addHeader("SOAPAction", "\"" + soapAction + "\"");
        Service webservice = Service.create(service);
        webservice.addPort(port, SOAPBinding.SOAP11HTTP_BINDING, endpointAddress);
        Dispatch<SOAPMessage> dispatch = webservice.createDispatch(port, SOAPMessage.class, Service.Mode.MESSAGE);

        SOAPMessage responseMessage = dispatch.invoke(requestMessage);
        System.out.println(XML.toString(responseMessage.getSOAPBody()));

    }

    public SOAPMessage invoke(SOAPMessage request) {
        try {

            String operation = request.getSOAPBody().getFirstChild().getLocalName();
            System.out.println("Operation " + operation + "has been invoked");

            QName service = new QName("http://www.itu.dk/research/pls/bpl/bpel/samples/wsdl", "bookingService");
            QName port = new QName("http://www.itu.dk/research/pls/bpl/bpel/samples/wsdl", "bookingPort");
            String endpointAddress = "http://localhost:8008/samples/booking";
            String soapAction = "http://www.itu.dk/research/pls/bpl/bpel/samples/wsdl/book";

            MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
            SOAPMessage responseMessage = factory.createMessage();
            SOAPPart soap = responseMessage.getSOAPPart();
            SOAPEnvelope envelope = soap.getEnvelope();
            SOAPBody body = envelope.getBody();

            SOAPElement content = body.addBodyElement(envelope
                    .createName(operation + "Response", "rpc", "http://www.itu.dk/research/pls/bpl/bpel/samples/wsdl"));
            SOAPElement beginsPartAccessorElement = content.addChildElement("ok");
            beginsPartAccessorElement.setTextContent("true");

            return responseMessage;

        } catch (Exception exception) {
            return null;
        }
    }

}
