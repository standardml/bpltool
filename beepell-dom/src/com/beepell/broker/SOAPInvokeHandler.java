package com.beepell.broker;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.Fault;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.PortType;
import javax.wsdl.extensions.soap.SOAPBinding;
import javax.xml.namespace.QName;
import javax.xml.soap.Detail;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPConstants;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPEnvelope;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFault;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Response;
import javax.xml.ws.Service;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import com.beepell.repository.ServiceRepository;

/**
 * @author Tim Hallwyl
 * 
 */
public class SOAPInvokeHandler implements Future<Message> {

    private final ServiceRepository services;

    private final Message request;

    private final Dispatch<SOAPMessage> dispatch;

    private SOAPMessage soapRequest;

    private Response<SOAPMessage> response;

    private Exception exception;

    /**
     * Used ONLY to tell when a one-way invocation has completed.
     */
    private boolean done = false;

    SOAPInvokeHandler(Message request, ServiceRepository services) {
        this.request = request;
        this.services = services;

        /*
         * Implementation note: Because invocations are allowed to use endpoint
         * addresses that are not specified in a WSDL document, we do not have
         * real service and port name to use. As we only use the Service object
         * once, the names we choose are of no importance. We use the port type
         * name for the port and the binding name for the service.
         */
        Service service = Service.create(request.getBindingName());
        service.addPort(request.getPortTypeName(), javax.xml.ws.soap.SOAPBinding.SOAP11HTTP_BINDING, request.getEndpoint().toString());
        this.dispatch = service.createDispatch(request.getPortTypeName(), SOAPMessage.class, Service.Mode.MESSAGE);

        try {
            this.soapRequest = convert(request);
        } catch (Exception exception) {
            this.exception = exception;
            return;
        }

        if (!isOneWay())
            this.response = this.dispatch.invokeAsync(this.soapRequest);

    }

    /**
     * Returns true if the operation binding is one-way. If the operation
     * binding is a request-response pattern, this method returns false.
     * 
     * @return true if this is a one-way operation. Otherwise false.
     */
    private boolean isOneWay() {
        BindingOperation bindingOperation = this.services.getBindingOperation(this.request.getBindingName(), this.request.getOperationName());

        if (bindingOperation.getBindingOutput() == null)
            return true;

        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.concurrent.Future#cancel(boolean)
     */
    @Override
    public boolean cancel(boolean mayInterruptIfRunning) {
        if (this.response == null)
            return false;

        return this.response.cancel(mayInterruptIfRunning);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.concurrent.Future#get()
     */
    @Override
    public Message get() throws InterruptedException, ExecutionException {
        if (this.exception != null)
            throw new ExecutionException(this.exception);

        try {
            if (this.response == null) {
                // One-way invocation.
                this.dispatch.invokeOneWay(this.soapRequest);
                this.done = true;
                return null;
            }
            // Request-response invocation
            SOAPMessage soapResponse = this.response.get();

            return convert(soapResponse);
        } catch (Exception exception) {
            throw new ExecutionException(exception);
        }
    }

    /**
     * Note: For one-way invocations the time-out is ignored, as it is not
     * supported by JAX-WS.
     * 
     * @see java.util.concurrent.Future#get(long, java.util.concurrent.TimeUnit)
     */
    @Override
    public Message get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
        if (this.exception != null)
            throw new ExecutionException(this.exception);

        try {
            if (this.response == null) {
                // One-way invocation.
                this.dispatch.invokeOneWay(this.soapRequest);
                this.done = true;
                return null;
            }

            // Request-response invocation
            SOAPMessage soapResponse = this.response.get(timeout, unit);
            return convert(soapResponse);
        } catch (Exception exception) {
            throw new ExecutionException(exception);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.concurrent.Future#isCancelled()
     */
    @Override
    public boolean isCancelled() {
        if (this.response == null)
            return false;

        return this.response.isCancelled();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.concurrent.Future#isDone()
     */
    @Override
    public boolean isDone() {
        if (this.response == null)
            return this.done;

        return this.response.isDone();
    }

    /**
     * Converts a SOAP response message into a Message.
     * 
     * @param response
     * @return An abstract message.
     * @throws SOAPException
     */
    @SuppressWarnings("unchecked")
    private Message convert(SOAPMessage response) throws SOAPException {
        Binding binding = this.services.getBinding(this.request.getBindingName());
        SOAPBinding soapBinding = ServiceRepository.getExtension(binding.getExtensibilityElements(), SOAPBinding.class);
        String style = soapBinding.getStyle().toLowerCase();

        try {
            response.writeTo(System.out);
        } catch (Exception exception) {
            exception.printStackTrace();
        }

        SOAPBody body = response.getSOAPBody();

        if (body.getFault() != null) {
            return handleFault(body.getFault());
        }

        Message message = new Message(this.request.getDefinitionName(), this.request.getPortTypeName(), this.request.getOperationName(), this.request
                .getBindingName(), this.request.getEndpoint());
        /*
         * Document / literal style Lazy implementation: we do not check the
         * name on the body child, we just used it as it is.
         */
        if (style.equals("document")) {
            Iterator iterator = body.getChildElements();

            // If no parts are expected, we're done.
            if (!iterator.hasNext())
                return message;

            // Message contain one or more parts. Note, at most one part may be
            // expected.
            Element bodyChild = (Element) iterator.next();
            message.getParts().put(bodyChild.getLocalName(), bodyChild);

            if (iterator.hasNext())
                throw new SOAPException("WS-I R2210, R2201, R2712, R9981: A document/literal message may contain at most one part.");
        }

        /*
         * RPC/literal binding
         */
        if (style.equals("rpc")) {

            QName wrapperQName;
            wrapperQName = new QName(this.request.getPortTypeName().getNamespaceURI(), this.request.getOperationName() + "Response");
            SOAPElement wrapperElement = (SOAPElement) body.getChildElements(wrapperQName).next();

            if (wrapperElement == null)
                throw new SOAPException("Failed to find the expected '" + wrapperQName + "' in the message.");

            Iterator iterator = wrapperElement.getChildElements();
            Element partAccessorElement;

            while (iterator.hasNext()) {
                partAccessorElement = (Element) iterator.next();
                message.getParts().put(partAccessorElement.getLocalName(), partAccessorElement);
            }
        }

        return message;
    }

    /**
     * Converts a request Message into a SOAPMessage.
     * 
     * @param request
     * @return A SOAP message.
     */
    private SOAPMessage convert(Message request) throws SOAPException {
        Binding binding = this.services.getBinding(this.request.getBindingName());
        SOAPBinding soapBinding = ServiceRepository.getExtension(binding.getExtensibilityElements(), SOAPBinding.class);
        String style = soapBinding.getStyle().toLowerCase();

        Map<String, Node> parts = request.getParts();

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

            if (request.getPortTypeName().getNamespaceURI() != null) {
                wrapperElement = body.addBodyElement(envelope.createName(request.getOperationName(), "rpc", request.getPortTypeName().getNamespaceURI()));
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
                    Node variableNodeCopy = wrapperElement.getOwnerDocument().importNode(partNode, true);
                    variableNodeCopy.getOwnerDocument().renameNode(variableNodeCopy, null, partName);
                    wrapperElement.appendChild(variableNodeCopy);
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

    /**
     * The fault message MUST have a single part. The use, encodingStyle and
     * namespace attributes are all used in the same way as with soap:body (see
     * section 3.5), only style="document" is assumed since faults do not
     * contain parameters. [WSDL 3.6]
     * 
     * @return An Message representing the fault.
     * @throws SOAPException If anything went wrong.
     */
    @SuppressWarnings("unchecked")
    private Message handleFault(SOAPFault soapFault) throws SOAPException {

        Detail detail = soapFault.getDetail();
        if (detail == null)
            throw new SOAPException("A undefined fault message was returned.");

        /*
         * A fault message, contained in the details of a SOAPFault, is ASSUMED
         * a document/literal bound message [WSDL 3.6] – and as such is may
         * have at most one part, and this part must be referring an element
         * declaration. R2204, R2205, R2206. We support only document/literal
         * bound fault message.
         */
        Iterator<SOAPElement> iterator = detail.getChildElements();
        if (iterator.hasNext()) {
            /*
             * The fault have something in the detail: We expect only one
             * element, and lookup if this one element matches a fault message
             * part (fault messages have at exactly one part).
             */
            SOAPElement element = iterator.next();
            QName elementName = new QName(element.getNamespaceURI(), element.getLocalName());

            Fault fault = getFault(elementName, this.request.getPortTypeName(), this.request.getOperationName());

            QName faultName = new QName(this.request.getPortTypeName().getNamespaceURI(), fault.getName());
            QName faultMessageType = fault.getMessage().getQName();

            Message faultMessage = new Message(faultMessageType, this.request.getPortTypeName(), this.request.getOperationName(),
                    this.request.getBindingName(), this.request.getEndpoint());

            faultMessage.getParts().put(faultName.getLocalPart(), (Element) soapFault.getDetail().getChildElements().next());

            return faultMessage;
        }

        throw new SOAPException("A undefined fault message was returned.");

    }

    /**
     * Returns the WSDL fault definition matching the faultElement, port type
     * and operation name.
     * 
     * @param faultElement The QName of the first child element of the fault
     *            details.
     * @param portTypeName The port type definition the fault message was sent to.
     * @param operationName The name of the operation the fault message was sent
     *            to.
     * @return The WSDL fault definition.
     */
    @SuppressWarnings("unchecked")
    private Fault getFault(QName faultElement, QName portTypeName, String operationName) {

        PortType portType = this.services.getPortType(portTypeName);
        Operation operation = portType.getOperation(operationName, null, null);
        Collection<Fault> faults = operation.getFaults().values();

        for (Fault fault : faults) {
            String partName = ((Part) fault.getMessage().getOrderedParts(null).get(0)).getName();
            Part part = fault.getMessage().getPart(partName);
            if (faultElement.equals(part.getElementName()))
                return fault;
        }

        return null;
    }

}
