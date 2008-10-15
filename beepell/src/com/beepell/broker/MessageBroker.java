package com.beepell.broker;

import java.util.Iterator;
import java.util.List;
import java.util.concurrent.Future;
import java.util.logging.Logger;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.PortType;
import javax.wsdl.extensions.soap.SOAPBinding;
import javax.wsdl.extensions.soap.SOAPBody;
import javax.wsdl.extensions.soap.SOAPOperation;
import javax.xml.namespace.QName;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Service;

import org.w3c.dom.DOMException;

import com.beepell.Settings;
import com.beepell.activity.basic.InboundMessageActivity;
import com.beepell.activity.basic.Invoke;
import com.beepell.deployment.ProcessContext;
import com.beepell.exceptions.CorrelationViolation;
import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.SelectionFailure;
import com.beepell.exceptions.SubLanguageExecutionFault;
import com.beepell.exceptions.UninitializedPartnerRole;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.execution.CorrelationSet;
import com.beepell.execution.EndpointReference;
import com.beepell.execution.ExecutionContext;
import com.beepell.execution.PartnerLink;
import com.beepell.model.Correlation;
import com.beepell.model.CorrelationWithPattern;
import com.beepell.model.InitiateEnumeration;
import com.beepell.model.PatternEnumeration;
import com.beepell.repository.PartnerLinkType;
import com.beepell.repository.ServiceRepository;
import com.beepell.variable.MessageVariable;

/**
 * @author Tim Hallwyl
 */
public class MessageBroker {
     
    private static final Logger log = Settings.getLogger();
    
    /**
     * Used by invoke-activities to call Web Services offered by service
     * providers. Operations can be request-response or one-way operations,
     * corresponding to WSDL 1.1 operation definitions.
     * <p>
     * One-way invocation requires only the inputVariable since a response is
     * not expected as part of the operation.
     * <p>
     * Request-response invocation requires both an inputVariable and an
     * outputVariable.
     * <p>
     * If a WSDL message definition does not contain any parts, then the
     * associated arguments, input or output, MAY be null.
     * 
     * @param invoke the invoke activity
     * @param context the execution context used by the invoke activity
     * @return a future to optionally cancel before completion. Note, it may NOT
     *         be used to fetch the response.
     * @throws InvalidExpressionValue
     * @throws SubLanguageExecutionFault
     * @throws SelectionFailure
     * @throws CorrelationViolation
     * @throws UninitializedVariable
     * @throws UninitializedPartnerRole 
     */
    public static Future<?> invoke(final Invoke invoke, final ExecutionContext context) throws CorrelationViolation, SelectionFailure, SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable, UninitializedPartnerRole {
        final String partnerLinkName = invoke.getPartnerLink();
        final String operationName = invoke.getOperation();
        final String inputVariableName = invoke.getInputVariable();

        
        /*
         * If the correlation constraints (both initiate and consistency) are
         * violated, then the message must NOT be sent, and a fault must be
         * thrown.
         */
        invokeRequestCorrelation(invoke.getCorrelations(), inputVariableName, context);

        try {
            ServiceRepository repository = context.getServiceRepository();

            PartnerLink partnerLink = context.getPartnerLink(partnerLinkName);

            // Get the PartnerLinkType
            PartnerLinkType partnerLinkType = context.getServiceRepository().getPartnerLinkType(partnerLink.getPartnerLinkType());

            // Get the PortType
            PortType portType = partnerLinkType.getPortType(partnerLink.getPartnerRole());

            // Get the Binding
            Binding binding = repository.getBinding(portType.getQName());

            // Check SOAP Binding
            SOAPBinding soapBinding = ServiceRepository.getExtension(binding.getExtensibilityElements(), SOAPBinding.class);

            if (!soapBinding.getTransportURI().equals("http://schemas.xmlsoap.org/soap/http"))
                throw new SOAPException("WS-I R2701, R2702: Only HTTP transport protocol is supported.");

            String style = "document";
            if (soapBinding.getStyle() != null)
                style = soapBinding.getStyle();

            // Get SOAP Operation Binding
            BindingOperation wsdlOperationBinding = getOperation(operationName, binding.getBindingOperations());
            SOAPOperation soapOperationBinding = ServiceRepository.getExtension(wsdlOperationBinding.getExtensibilityElements(), SOAPOperation.class);

            if (soapOperationBinding.getStyle() != null)
                style = soapOperationBinding.getStyle();

            String soapAction = soapOperationBinding.getSoapActionURI();

            final boolean isOneWayOperation = (wsdlOperationBinding.getOperation().getOutput() == null);
            
            /*
             * Create request message and response handler.
             */
            SOAPMessage requestMessage;
            InvokeResponseHandler responseHandler = null;

            // Setting up variables
            MessageVariable inputVariable = null;
            if (inputVariableName != null)
                inputVariable = (MessageVariable) context.getVariable(inputVariableName);

            if (style.equals("rpc")) {
                String OperationInputNamespace = getOperationNamespace(wsdlOperationBinding.getBindingInput());
                

                QName operation = new QName(OperationInputNamespace, operationName);
                requestMessage = MessageBinding.bind(inputVariable, style, operation, MessageBinding.REQUEST);

                if (!isOneWayOperation) {
                    String OperationOutputNamespace = getOperationNamespace(wsdlOperationBinding.getBindingOutput());
                    operation = new QName(OperationOutputNamespace, operationName);
                    responseHandler = new InvokeResponseHandler(invoke, context, portType, style, operation);
                }

            } else {
                // document style
                QName operation = new QName(operationName);
                requestMessage = MessageBinding.bind(inputVariable, style, operation, MessageBinding.REQUEST);
                responseHandler = new InvokeResponseHandler(invoke, context, portType, style, operation);

            }

            // Add SOAP Action header
            requestMessage.getMimeHeaders().addHeader("SOAPAction", "\"" + soapAction + "\"");

            // dispatch message (invoke)
            Dispatch<SOAPMessage> dispatch = getDispatch(partnerLink.getPartnerEndpoint());
            if (isOneWayOperation) {
                dispatch.invokeOneWay(requestMessage);
                invoke.messageReceived();
                return null;
            }
            else
                return dispatch.invokeAsync(requestMessage, responseHandler);

        } catch (SOAPException exception) {
            exception.printStackTrace();
            // TODO throw something

        } catch (DOMException exception) {
            exception.printStackTrace();
            // TODO throw something

        } catch (UninitializedVariable exception) {
            throw exception;

        } catch (UninitializedPartnerRole exception) {
            throw exception;
        }

        return null;

    }

    private static String getOperationNamespace(BindingInput bindingInput) throws SOAPException {
        SOAPBody body = ServiceRepository.getExtension(bindingInput.getExtensibilityElements(), SOAPBody.class);
        String namespace = body.getNamespaceURI();
        String use = body.getUse();

        if (body.getEncodingStyles() != null && body.getEncodingStyles().size() > 0)
            throw new SOAPException("WS-I R2706: The Profile prohibits the use of encodings, including the SOAP encoding.");

        if (!use.equals("literal"))
            throw new SOAPException("WS-I R2706: Only 'literal' usage is supported.");

        return namespace;
    }

    private static String getOperationNamespace(BindingOutput bindingOutput) throws SOAPException {
        SOAPBody body = ServiceRepository.getExtension(bindingOutput.getExtensibilityElements(), SOAPBody.class);
        String namespace = body.getNamespaceURI();
        String use = body.getUse();

        if (body.getEncodingStyles() != null && body.getEncodingStyles().size() > 0)
            throw new SOAPException("WS-I R2706: The Profile prohibits the use of encodings, including the SOAP encoding.");

        if (!use.equals("literal"))
            throw new SOAPException("WS-I R2706: Only 'literal' usage is supported.");

        return namespace;
    }

    private static Dispatch<SOAPMessage> getDispatch(EndpointReference endpoint) throws UninitializedPartnerRole {
        /*
         * Implementation note: The Service.create should have the wsdl:service
         * QName, but this is unavailable since our endpoint implementation only
         * carry the wsdl:port element. Even if it did carry the wsdl:service,
         * the target namespace would be unavailable, thus only the local name
         * would be known. Instead we use the binding name -- it has no effect
         * on the wire.
         */
        //Service webservice = Service.create(endpoint.getService());
        //webservice.addPort(endpoint.getPort(), javax.xml.ws.soap.SOAPBinding.SOAP11HTTP_BINDING, endpoint.getLocation());
        //return webservice.createDispatch(endpoint.getPort(), SOAPMessage.class, Service.Mode.MESSAGE);
        Service webservice = Service.create(endpoint.getBinding());
        webservice.addPort(endpoint.getPort(), javax.xml.ws.soap.SOAPBinding.SOAP11HTTP_BINDING, endpoint.getLocation());
        return webservice.createDispatch(endpoint.getPort(), SOAPMessage.class, Service.Mode.MESSAGE);

    }

    /*
     * Note: WS-BPEL does NOT support overloaded operations, so we only need to
     * match the name.
     */
    private static BindingOperation getOperation(final String name, final List list) {

        Iterator iterator = list.iterator();
        while (iterator.hasNext()) {
            BindingOperation operation = (BindingOperation) iterator.next();
            if (operation.getName().equals(name))
                return operation;
        }
        return null;
    }

    /**
     * Correlation initiation and consistency constraints of a request message.
     * 
     * @param correlations the correlation set that apply -- only REQUEST or
     *            REQUEST_RESPONSE or unspecified set are evaluated.
     * @param inputVariable message variable name
     * @param context the execution context to use for evaluation / initiation
     * @throws CorrelationViolation
     * @throws SelectionFailure
     * @throws SubLanguageExecutionFault
     * @throws InvalidExpressionValue
     * @throws UninitializedVariable
     */
    static void invokeRequestCorrelation(final List<CorrelationWithPattern> correlations, final String inputVariable, final ExecutionContext context) throws CorrelationViolation, SelectionFailure, SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable {
        if (correlations == null)
            return;
        
        boolean initiate, join;
        CorrelationSet correlationSet;
        for (CorrelationWithPattern correlation : correlations) {
            correlationSet = context.getCorrelationSet(correlation.getSet());

            initiate = join = false; // initiate default is NO
            if (correlation.getInitiate() == InitiateEnumeration.YES) {
                join = false;
                initiate = true;
            }
            if (correlation.getInitiate() == InitiateEnumeration.JOIN) {
                join = true;
                initiate = true;
            }

            if (correlation.getPattern() != PatternEnumeration.RESPONSE) {
                // pattern == REQUEST or REQUEST_RESPONSE or null (one-way)
                if (initiate)
                    correlationSet.initiate(join, inputVariable, context);

                correlationSet.checkConsistency(inputVariable, context);
            }
        }
    }

    /**
     * Package level access. Used by ResponseHandler.
     * 
     * @throws UninitializedVariable
     */
    static void invokeResponseCorrelation(final List<CorrelationWithPattern> correlations, final String outputVariable, final ExecutionContext context) throws CorrelationViolation, SelectionFailure, SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable {
        if (correlations == null)
            return;
        
        boolean initiate, join;
        CorrelationSet correlationSet;
        for (CorrelationWithPattern correlation : correlations) {
            correlationSet = context.getCorrelationSet(correlation.getSet());

            initiate = join = false; // initiate default is NO
            if (correlation.getInitiate() == InitiateEnumeration.YES) {
                join = false;
                initiate = true;
            }
            if (correlation.getInitiate() == InitiateEnumeration.JOIN) {
                join = true;
                initiate = true;
            }

            if (correlation.getPattern() == PatternEnumeration.RESPONSE || correlation.getPattern() == PatternEnumeration.REQUEST_RESPONSE) {
                if (initiate)
                    correlationSet.initiate(join, outputVariable, context);

                correlationSet.checkConsistency(outputVariable, context);
            }
        }
    }

    
    private static InboundMessageHandler getIMH(InboundMessageActivity ima) {
        ExecutionContext context = ima.getContext();
        PartnerLink partnerLink = context.getPartnerLink(ima.getPartnerLink());
        PartnerLinkType partnerLinkType = context.getServiceRepository().getPartnerLinkType(partnerLink.getPartnerLinkType());
        PortType portType = partnerLinkType.getPortType(partnerLink.getMyRole());
        return SOAPServer.getHandler(portType.getQName());
    }
    
    /**
     * Add an inbound message activity to the list, waiting for a message.
     * 
     * @param ima
     */
    public static void receive(InboundMessageActivity ima) {
        InboundMessageHandler imh = getIMH(ima);
        imh.add(ima);
        log.info("Added IMA to partner link '" + ima.getPartnerLink() + "' (myRole) handler.");
    }

    /**
     * Cancel the inbound message activity; it is no longer waiting.
     * 
     * @param ima
     */
    public static void cancel(InboundMessageActivity ima) {
        InboundMessageHandler imh = getIMH(ima);
        imh.remove(ima);
        log.info("Cancelled IMA at partner link '" + ima.getPartnerLink() + "' myRole.");
    }

    /**
     * Correlation initiation and consistency constraints of a receive (request)
     * or a reply (response) message.
     * 
     * TODO Use this or delete it!
     * 
     * @param correlations
     * @param variable
     * @param context
     * @throws CorrelationViolation
     * @throws SelectionFailure
     * @throws SubLanguageExecutionFault
     * @throws InvalidExpressionValue
     * @throws UninitializedVariable
     */
    @SuppressWarnings("unused")
    private static void correlation(final List<Correlation> correlations, final String variable, final ExecutionContext context) throws CorrelationViolation, SelectionFailure, SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable {
        boolean initiate, join;
        CorrelationSet correlationSet;
        for (Correlation correlation : correlations) {
            correlationSet = context.getCorrelationSet(correlation.getSet());

            initiate = join = false; // initiate default is NO
            if (correlation.getInitiate() == InitiateEnumeration.YES) {
                join = false;
                initiate = true;
            }
            if (correlation.getInitiate() == InitiateEnumeration.JOIN) {
                join = true;
                initiate = true;
            }

            if (initiate)
                correlationSet.initiate(join, variable, context);

            correlationSet.checkConsistency(variable, context);
        }
    }

    /**
     * Publish the process.
     * 
     * @param context
     * @throws Exception
     */
    public static void publish(ProcessContext context) throws Exception {
        SOAPServer.publish(context);
    }

    /**
     * Unpublish the process.
     * 
     * @param context
     */
    public static void unpublish(ProcessContext context) {
        SOAPServer.unpublish(context);
    }

}
