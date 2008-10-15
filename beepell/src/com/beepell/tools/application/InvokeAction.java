package com.beepell.tools.application;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.xml.namespace.QName;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPConstants;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPEnvelope;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Service;
import javax.xml.ws.soap.SOAPBinding;

import com.beepell.deployment.DeploymentManager;
import com.beepell.deployment.ProcessContext;

/**
 * @author Tim Hallwyl
 * 
 */
public class InvokeAction extends AbstractAction {

    private final Component component;

    public InvokeAction(Component parent) {
        this.component = parent;

        ImageIcon icon = new ImageIcon(InvokeAction.class.getResource("invoke.png"), "Invoke Sample Process");

        this.putValue(LARGE_ICON_KEY, icon);
        this.putValue(SMALL_ICON, icon);
        this.putValue(NAME, "Invoke Process");
        this.putValue(SHORT_DESCRIPTION, "Invoke sample process");
        this.putValue(LONG_DESCRIPTION, "Invokes the sample process.");

    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        Thread thread = new Thread() {
            public synchronized void run() {
                try {
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
                    currencyPartAccessorElement.setTextContent("EUR");

                    SOAPElement amountPartAccessorElement = content.addChildElement("amount");
                    amountPartAccessorElement.setTextContent(String.valueOf(430));

                    requestMessage.getMimeHeaders().addHeader("SOAPAction", "\"" + soapAction + "\"");
                    Service webservice = Service.create(service);
                    webservice.addPort(port, SOAPBinding.SOAP11HTTP_BINDING, endpointAddress);
                    Dispatch<SOAPMessage> dispatch = webservice.createDispatch(port, SOAPMessage.class, Service.Mode.MESSAGE);
                    @SuppressWarnings("unused")
                    SOAPMessage responseMessage = dispatch.invoke(requestMessage);
                } catch (Exception exception) {
                    JOptionPane.showMessageDialog(component, exception.getLocalizedMessage(), "Invoke Failed", JOptionPane.WARNING_MESSAGE);
                }
            }
        };

        thread.start();
    }

}
