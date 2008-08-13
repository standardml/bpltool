package com.beepell.tools.launch;

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
import javax.xml.ws.Service;
import javax.xml.ws.soap.SOAPBinding;

import com.beepell.Settings;
import com.beepell.deployment.DeploymentManager;
import com.beepell.deployment.ProcessContext;

/**
 * A Java application to deploy a process.
 * 
 * @author Tim Hallwyl
 */
public class Launch {

    /**
     * Deploy a process or run an example.
     * <p>
     * Usage: Launch process.bpel
     * 
     * @param args
     */
    public static void main(String[] args) {
        Settings settings = Settings.getInstance();
        settings.setSetting("tools.tracker.enabled", "true");
        settings.setSetting("tools.tracker.mode", "step");

        ProcessContext process = null;        
        try {
            if (args.length == 0) {
                /* Run the exchange test case */
                
                URL url = Launch.class.getResource("/com/beepell/tools/launch/simple.bpel");
                
                File bpel = new File(url.toURI());
                process = deploy(bpel);
                Thread.sleep(500);
                invoke("EUR", 100.00f);
                Thread.sleep(30000);
            } else {
                long time = Long.MAX_VALUE;
                File bpel = new File(args[0]);
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

    private static ProcessContext deploy(File bpel) throws Exception {
        ProcessContext process = DeploymentManager.deploy(bpel);
        System.out.println("Launch: Process has been deployed.");
        return process;
    }

    private static void invoke(String currency, Float amount) throws SOAPException, IOException {
        QName service = new QName("http://beepell.com/tests/exchange/wsdl", "exchangeService");
        QName port = new QName("http://beepell.com/tests/exchange/wsdl", "exchangePortType");
        String endpointAddress = "http://localhost:9090/samples/proxy/definitions/exchange";
        String soapAction = "http://beepell.com/tests/exchange/wsdl";

        MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
        SOAPMessage requestMessage = factory.createMessage();
        SOAPPart soap = requestMessage.getSOAPPart();
        SOAPEnvelope envelope = soap.getEnvelope();
        SOAPBody body = envelope.getBody();
        SOAPElement content = body.addBodyElement(envelope.createName("exchangeOperation", "rpc", "http://beepell.com/tests/exchange/wsdl"));

        SOAPElement currencyPartAccessorElement = content.addChildElement("currency");
        currencyPartAccessorElement.setTextContent(currency);

        SOAPElement amountPartAccessorElement = content.addChildElement("amount");
        amountPartAccessorElement.setTextContent(String.valueOf(amount));

        requestMessage.getMimeHeaders().addHeader("SOAPAction", "\"" + soapAction + "\"");
        Service webservice = Service.create(service);
        webservice.addPort(port, SOAPBinding.SOAP11HTTP_BINDING, endpointAddress);
        Dispatch<SOAPMessage> dispatch = webservice.createDispatch(port, SOAPMessage.class, Service.Mode.MESSAGE);
        @SuppressWarnings("unused")
        SOAPMessage responseMessage = dispatch.invoke(requestMessage);

    }

}
