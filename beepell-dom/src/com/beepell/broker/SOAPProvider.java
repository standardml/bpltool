package com.beepell.broker;

import java.util.Iterator;
import java.util.Map;

import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.Operation;
import javax.wsdl.extensions.soap.SOAPBinding;
import javax.xml.namespace.QName;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPConstants;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPEnvelope;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;
import javax.xml.ws.Service;
import javax.xml.ws.ServiceMode;
import javax.xml.ws.WebServiceProvider;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import com.beepell.repository.ServiceRepository;

/**
 * The Provider receives the request message and provides the response message,
 * if any. It is the responsibility of the Provider to convert the protocol
 * specific SOAP message into a independent Message object.
 * <p>
 * We implement this as a SOAPMessage provider because the determination of
 * operation name and message parts are specific to the SOAP binding use. Of
 * this reason we cannot use LogicalMessage.
 * 
 * 
 * @author Tim Hallwyl
 * 
 */
@WebServiceProvider
@ServiceMode(value = Service.Mode.MESSAGE)
public class SOAPProvider implements javax.xml.ws.Provider<SOAPMessage> {

    private final Broker broker;

    private final Binding binding;
    
    private final ServiceRepository services;

    /**
     * Creates a provider for the specified port type.
     * 
     * @param broker The Broker that handles the requests.
     * @param binding The binding to use.
     * @param services The service repository with the definitions to use.
     */
    public SOAPProvider(Broker broker, QName binding, ServiceRepository services) {
        this.broker = broker;
        this.binding = services.getBinding(binding);
        this.services = services;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.xml.ws.Provider#invoke(java.lang.Object)
     */
    @Override
    public SOAPMessage invoke(SOAPMessage message) {
        try {
            String operationName = getOperationName(message);

            // TODO 0. Determine if target operation is one-way or
            // request-response
            boolean requestResponse = isRequestResponse(operationName);

            // TODO 1. Convert to internal message
            Message request = convert(message, operationName);

            if (requestResponse) {
                Message response = this.broker.getResponse(request);
                return convert(response);
            }

            // One-way interaction.
            this.broker.addMessage(request);
            return null;
        } catch (Exception exception) {
            // TODO Add error handling.
            return null;
        }
    }
    
    private boolean isRequestResponse(String operation) {
        BindingOperation bindingOperation = this.services.getBindingOperation(this.binding.getQName(), operation);

        if (bindingOperation.getBindingOutput() != null)
            return true;

        return false;
    }
    
    
    @SuppressWarnings("unchecked")
    private Message convert(SOAPMessage soapRequest, String operationName) throws SOAPException {
        Operation operation = this.binding.getPortType().getOperation(operationName, null, null);
        Message message = new Message(operation.getInput().getMessage().getQName(), this.binding.getPortType().getQName(), operationName, this.binding.getQName(), null);

        SOAPBinding soapBinding = ServiceRepository.getExtension(this.binding.getExtensibilityElements(), SOAPBinding.class);
        String style = soapBinding.getStyle().toLowerCase();
        
        SOAPBody body = soapRequest.getSOAPBody();

        // TODO Implement fault handling ?
        if (body.getFault() != null)
            return null;

        /*
         * Document / literal style Lazy implementation: we do not check the
         * name on the body child, we just used it as it is.
         */
        if (style.equals("document")) {
            Iterator<SOAPElement> iterator = body.getChildElements();

            // If no parts are expected, we're done.
            if (!iterator.hasNext())
                return message;

            // Message contain one or more parts. Note, at most one part may be
            // expected.
            Element bodyChild = iterator.next();
            message.getParts().put(bodyChild.getLocalName(), bodyChild);

            if (iterator.hasNext())
                throw new SOAPException("WS-I R2210, R2201, R2712, R9981: A document/literal message may contain at most one part.");
        }

        /*
         * RPC/literal binding
         */
        if (style.equals("rpc")) {

            QName wrapperQName;
            wrapperQName = new QName(this.binding.getQName().getNamespaceURI(), operationName);
            SOAPElement wrapperElement = (SOAPElement) body.getChildElements(wrapperQName).next();

            if (wrapperElement == null)
                throw new SOAPException("Failed to find the expected '" + wrapperQName + "' in the message.");

            Iterator<Element> iterator = wrapperElement.getChildElements();
            Element partAccessorElement;

            while (iterator.hasNext()) {
                partAccessorElement = iterator.next();
                message.getParts().put(partAccessorElement.getLocalName(), partAccessorElement);
            }
        }

        
        return message;
    }

    /**
     * 
     * @param source
     * @return
     * @throws SOAPException 
     */
    private SOAPMessage convert(Message response) throws SOAPException {
        Binding binding = this.services.getBinding(response.getBindingName());
        SOAPBinding soapBinding = ServiceRepository.getExtension(binding.getExtensibilityElements(), SOAPBinding.class);
        String style = soapBinding.getStyle().toLowerCase();

        Map<String, Node> parts = response.getParts();

        MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
        SOAPMessage message = factory.createMessage();

        SOAPPart soap = message.getSOAPPart();
        SOAPEnvelope envelope = soap.getEnvelope();
        // SOAPHeader header = envelope.getHeader();
        SOAPBody body = envelope.getBody();

        /*
         * WS-I requires for rpc/literal style that message parts refer to type
         * declarations, not elements. Each part is wrapped in a part accessor
         * element whose local names is the name of the part and it is not
         * namespace qualified.
         */
        if (style.equals("rpc")) {

            SOAPBodyElement wrapperElement;

            if (response.getPortTypeName().getNamespaceURI() != null) {
                wrapperElement = body.addBodyElement(envelope.createName(response.getOperationName() + "Response", "rpc", response.getPortTypeName().getNamespaceURI()));
            } else
                throw new SOAPException("WS-I R1014, R2717: A RPC/literal message must declare an operation namespace in soap:body.");

            for (String partName : parts.keySet()) {
                /*
                 * Create an unqualified part accessor element.
                 */
                Node partNode = parts.get(partName);

                if (partNode instanceof Text) {
                    // Simple type part variable
                    SOAPElement partAccessorElement = wrapperElement.addChildElement(partName);
                    partAccessorElement.setTextContent(partNode.getTextContent());
                } else {
                    // Complex type part variable
                    Node partNodeCopy = wrapperElement.getOwnerDocument().importNode(partNode, true);
                    partNodeCopy.getOwnerDocument().renameNode(partNodeCopy, null, partName);
                    wrapperElement.appendChild(partNodeCopy);
                }
            }

        } else {

            /*
             * DOCUMENT STYLE WS-I requires for document/literal style that
             * message parts refer elements (not types) and at most one part
             * must be used per message. The element that is referred by the
             * message part is copied as the only child of the SOAP body.
             */

            if (parts.size() > 1)
                throw new SOAPException("WS-I R2210, R2201, R2712, R9981: A document/literal message may contain at most one part.");

            for (String partName : parts.keySet()) {
                if (parts.get(partName) instanceof Element) {
                    body.appendChild(body.getOwnerDocument().importNode(parts.get(partName), true));
                } else {
                    throw new SOAPException("WS-I R2204: A document/literal message part must refer to an element.");
                }
            }

        }

        message.saveChanges();
        return message;

    }

    private String getOperationName(SOAPMessage message) throws SOAPException {
        return ((SOAPElement) message.getSOAPBody().getChildElements().next()).getLocalName();
    }

}
