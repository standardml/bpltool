package com.beepell.broker;

import java.util.Collection;
import java.util.Iterator;
import java.util.concurrent.ExecutionException;

import javax.wsdl.Fault;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;
import javax.xml.soap.Detail;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPFault;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.AsyncHandler;
import javax.xml.ws.Response;
import javax.xml.ws.soap.SOAPFaultException;

import com.beepell.activity.basic.Invoke;
import com.beepell.exceptions.BPELFault;
import com.beepell.execution.ExecutionContext;
import com.beepell.variable.MessageVariable;

/**
 * Response Handler, receive message and notify listener.
 * 
 * @author Tim Hallwyl
 */
public class InvokeResponseHandler implements AsyncHandler<SOAPMessage> {

    private final MessageVariable variable;

    private final Invoke invoke;

    private final String style;

    private final QName operation;

    private final PortType portType;

    private final ExecutionContext context;

    /**
     * Create Response Handler, used only for Invoke.
     * 
     * @param invoke the invoke activity to receive the message
     * @param context execution context of the invoke activity
     * @param portType the WSDL PortType that declares the operation
     * @param style "rpc" or "document"
     * @param operation Operation BINDING QName - that is, the operations name
     *            and the namespace specified in the binding.
     */
    public InvokeResponseHandler(Invoke invoke, ExecutionContext context, PortType portType, String style, QName operation) {
        this.variable = (MessageVariable) context.getVariable(invoke.getOutputVariable());
        this.style = style;
        this.operation = operation;
        this.invoke = invoke;
        this.context = context;
        this.portType = portType;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.xml.ws.AsyncHandler#handleResponse(javax.xml.ws.Response)
     */
    public void handleResponse(Response<SOAPMessage> response) {
        try {
            /*
             * An invoke operation may be cancelled by the invoke activity, for
             * example if it is termination. In that case we simply return.
             */
            if (response.isCancelled())
                return;

            /*
             * If the invoke there is no response message -- not even a fault
             * message. Did we expect one?
             */
            SOAPMessage message = response.get();
            if (message == null) {
                if (variable != null) {
                    BPELFault fault = new MessagingFault("Recived null response on operation '" + this.operation + "'.");
                    invoke.failed(fault);
                } else {
                    // One-way operation: no response message was expected.
                    invoke.messageReceived();
                }
                return;
            }

            if (message.getSOAPBody().getFault() != null) {
                handleFault(message.getSOAPBody().getFault());
                return;
            }

            /*
             * We passed all fault checks: A regular response message was
             * received. We bind it to the outputVariable (if not null) and
             * notify the invoke activity.
             */
            if (variable != null)
                MessageBinding.bind(message, variable, style, operation, MessageBinding.RESPONSE);

            /*
             * When a bpel:correlationViolation is thrown by an invoke activity
             * because of a violation on the response of a request/response
             * operation, the response MUST be received before the
             * bpel:correlationViolation is thrown. [9.2]
             */
            try {

                MessageBroker.invokeResponseCorrelation(invoke.getCorrelations(), invoke.getOutputVariable(), context);

                
            } catch (BPELFault exception) {
                // Some correlation related fault, for example correlation
                // violation.
                invoke.failed(exception);
                return;
            }

            invoke.messageReceived();

        }  catch (ExecutionException exception) {
        	SOAPFaultException fault = (SOAPFaultException) exception.getCause();
            handleFault(fault.getFault());
            return;
        } catch (Exception exception) {
            // Something else has gone wrong...
            System.err.println("WARNING: Response handler failed.");
            BPELFault fault = new MessagingFault("Response handler failed:" + exception.getLocalizedMessage(), exception);
            invoke.failed(fault);
            return;
        }
    }

    /**
     * The fault message MUST have a single part. The use, encodingStyle and
     * namespace attributes are all used in the same way as with soap:body (see
     * section 3.5), only style="document" is assumed since faults do not
     * contain parameters. [WSDL 3.6]
     */
    private void handleFault(SOAPFault soapFault) {
        try {
            Detail detail = soapFault.getDetail();
            if (detail != null) {
                /*
                 * A fault message, contained in the details of a SOAPFault, is ASSUMED
                 * a document/literal bound message [WSDL 3.6] – and as such is may have at
                 * most one part, and this part must be referring an element
                 * declaration. R2204, R2205, R2206.
                 * We support only document/literal bound fault message.
                 */
                Iterator iterator = detail.getChildElements();
                if (iterator.hasNext()) {
                    /*
                     * The fault have something in the detail: We expect only
                     * one element, and lookup if this one element matches a
                     * fault message part (fault messages have at exactly one
                     * part).
                     */
                    SOAPElement element = (SOAPElement) iterator.next();
                    QName elementName = new QName(element.getNamespaceURI(), element.getLocalName());
                    Fault fault = getFault(elementName, portType, operation.getLocalPart());

                    QName faultName = new QName(portType.getQName().getNamespaceURI(), fault.getName());
                    QName faultMessageType = fault.getMessage().getQName();

                    MessageVariable variable = new MessageVariable(faultMessageType, faultName.getLocalPart(), context.getSchemaRepository(), context.getServiceRepository());

                    MessageBinding.bind(soapFault, variable);

                    invoke.failed(new FaultMessage(faultName, variable));
                    return;
                } else {
                    System.out.println("WARNING: Invoke response was a fault without fault details.");
                    System.out.println("WARNING: - Code '" + soapFault.getFaultCode() + "'");
                    System.out.println("WARNING: - Reason '" + soapFault.getFaultString() + "'");
                    invoke.failed(new MessagingFault("Invoke response was a fault without fault details."));
                }
            }
        } catch (Exception exception) {

            if (!(exception instanceof BPELFault))
                exception = new MessagingFault(exception);

            invoke.failed((BPELFault) exception);

        }

    }

    /**
     * Looking up the fault name matching the fault element QName.
     * <p>
     * &quot;This results in a fault identified in WS-BPEL by a QName formed by
     * the target namespace of the corresponding port type and the fault
     * name.&quot; [10.3]
     * 
     * TODO: Remove this method, if not used.
     * 
     * @param faultElement
     * @param portType
     * @param operationName
     * @return null if not matching fault was found, otherwise the fault QName.
     */
    @SuppressWarnings({ "unchecked", "unused" })
    private QName getFaultName(QName faultElement, PortType portType, String operationName) {

        Operation operation = portType.getOperation(operationName, null, null);
        Collection<Fault> faults = operation.getFaults().values();

        for (Fault fault : faults) {
            String partName = (String) fault.getMessage().getOrderedParts(null).get(0);
            Part part = fault.getMessage().getPart(partName);
            if (part.getElementName().equals(faultElement))
                return new QName(portType.getQName().getNamespaceURI(), fault.getName());
        }

        return null;
    }

    @SuppressWarnings("unchecked")
    private Fault getFault(QName faultElement, PortType portType, String operationName) {

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
