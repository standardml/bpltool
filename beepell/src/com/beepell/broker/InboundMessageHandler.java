package com.beepell.broker;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.wsdl.Binding;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.PortType;
import javax.wsdl.extensions.soap.SOAPBinding;
import javax.wsdl.extensions.soap.SOAPOperation;
import javax.xml.namespace.QName;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.Provider;
import javax.xml.ws.Service;
import javax.xml.ws.ServiceMode;
import javax.xml.ws.WebServiceProvider;

import org.w3c.dom.Element;

import com.beepell.Settings;
import com.beepell.activity.basic.InboundMessageActivity;
import com.beepell.activity.basic.Reply;
import com.beepell.deployment.ProcessContext;
import com.beepell.exceptions.BPELFault;
import com.beepell.exceptions.CorrelationViolation;
import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.SelectionFailure;
import com.beepell.exceptions.SubLanguageExecutionFault;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.execution.CorrelationSet;
import com.beepell.execution.ProcessInstance;
import com.beepell.execution.event.MessageEventHandler;
import com.beepell.model.Activity;
import com.beepell.model.Correlation;
import com.beepell.model.InitiateEnumeration;
import com.beepell.model.OnMessage;
import com.beepell.model.PickActivity;
import com.beepell.model.ReceiveActivity;
import com.beepell.repository.PartnerLinkType;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.tools.tracker.Tracker;
import com.beepell.variable.MessageVariable;

/**
 * An object of this class handles incoming request messages, only on the port
 * type specified in the constructor.
 * 
 * @author Tim Hallwyl
 */
@WebServiceProvider
@ServiceMode(value = Service.Mode.MESSAGE)
public class InboundMessageHandler implements Provider<SOAPMessage> {

    private final SchemaRepository schemas;

    private final ServiceRepository services;

    private final ProcessContext processContext;

    private final PartnerLinkType partnerLinkType;

    private final String role;

    private final PortType portType;

    private List<InboundMessageActivity> listeners = new ArrayList<InboundMessageActivity>();

    /**
     * This is a list of starting activities (ReceiveActivity or PickActivity)
     * that receives messages on this port type (partnerlinktype, role)
     */
    private List<Activity> startingActivities = null;

    /**
     * @param process
     * @param partnerLinkType
     * @param role
     */
    public InboundMessageHandler(ProcessContext process, PartnerLinkType partnerLinkType, String role) {
        if (process == null)
            throw new IllegalArgumentException("Null value passed to InboundMessageHandler constructor for ProcessContext argument");

        if (partnerLinkType == null)
            throw new IllegalArgumentException("Null value passed to InboundMessageHandler constructor for PartnerLinkType argument");

        if (role == null)
            throw new IllegalArgumentException("Null value passed to InboundMessageHandler constructor for role argument");

        this.processContext = process;
        this.schemas = process.getSchemaRepository();
        this.services = process.getServiceRepository();
        this.portType = partnerLinkType.getPortType(role);
        this.partnerLinkType = partnerLinkType;
        this.role = role;
        this.startingActivities = process.getStartActivities(partnerLinkType.getName(), role);

    }

    public SOAPMessage invoke(SOAPMessage request) {
        QName operation = getOperation(request);
        if (operation == null)
            throw new IllegalArgumentException("Operation not supported.");
        
        System.out.println("INFO: Received a message at Partner Link Type '" + partnerLinkType.getName().getLocalPart() + "' role '" + role + "' on operation '" + operation.getLocalPart() + "'.");

        InboundMessageActivity ima = findMatchingActivity(request);       
        if (ima != null) {
            try {
                if (ima instanceof MessageEventHandler) {
                    // TODO notify handler and wait for receive
                    MessageEventHandler handler = ((MessageEventHandler) ima);
                    ima = handler.messageEnqueued();
                    return deliver(request, ima, operation);
                }
                else
                   return deliver(request, ima, operation);
            
            } catch (BPELFault exception) {
                ima.failed(exception);
                // TODO: return SOAPFault
                return null;
            } catch (Exception exception) {
                exception.printStackTrace();
                ima.failed(new MessagingFault(exception));

                // TODO: return SOAPFault
                return null;
            }
            
        }

        else {
            // There is no IMA waiting for this message, let's see if
            // we shall create a new process instance.
            Activity startingActivity = findStartingActivity(operation);
            if (startingActivity != null) {
                ProcessInstance instance;
                // Create new process instance
                if (startingActivity instanceof ReceiveActivity)
                    instance = new ProcessInstance(processContext, (ReceiveActivity) startingActivity);
                else
                    instance = new ProcessInstance(processContext, (PickActivity) startingActivity, getOnMessage((PickActivity) startingActivity, operation));
                
                if (Settings.getInstance().getSetting("tools.tracker.enabled", "true").equals("true"))
                    Tracker.createInstance(instance);
                
                ima = instance.start();

                try {
                    return deliver(request, ima, operation);
                } catch (BPELFault exception) {
                    if (ima != null)
                        ima.failed(exception);
                    exception.printStackTrace();
                    // TODO: return SOAPFault
                    return null;
                } catch (Exception exception) {
                    if (ima != null)
                        ima.failed(new MessagingFault(exception));
                    exception.printStackTrace();
                    // TODO: return SOAPFault
                    return null;
                }

            } else {
                // TODO: return SOAPFault
                System.out.println("INFO: No starting activities matching message.");
                return null;
            }

        }

    }

    private SOAPMessage deliver(SOAPMessage requestMessage, InboundMessageActivity ima, QName operation) throws SOAPException, CorrelationViolation, UninitializedVariable, InvalidExpressionValue, SubLanguageExecutionFault, SelectionFailure {
        /*
         * Handle request
         */
        listeners.remove(ima);
        MessageVariable requestVariable = (MessageVariable) ima.getContext().getVariable(ima.getVariable());
        // TODO Write lock here?
        String style = getStyle(operation);
        MessageBinding.bind(requestMessage, requestVariable, style, operation, MessageBinding.REQUEST);
        initializeCorrelations(ima);
        Reply reply = ima.messageReceived();

        if (reply == null)
            return null;

        /*
         * Handle response
         * If it fails, we re-open the IMA and await an other reply.
         */
        while (true) {
            try {
                // TODO: Correlations!!
                MessageVariable response = (MessageVariable) reply.getContext().getVariable(reply.getVariable());
                SOAPMessage replyMessage = MessageBinding.bind(response, style, operation, MessageBinding.RESPONSE);
                reply.complete();
                return replyMessage;

            } catch (BPELFault exception) {
                reply.failed(exception);
                reply = ima.messageReceived();
            }
        }

    }

    private Activity findStartingActivity(QName operation) {
        ReceiveActivity receive;
        PickActivity pick;
        for (Activity startingActivity : startingActivities) {
            if (startingActivity instanceof ReceiveActivity) {
                receive = (ReceiveActivity) startingActivity;
                if (operation.getLocalPart().equals(receive.getOperation())) {
                    System.out.println("INFO: Message target is Receive activity '" + receive.getName() + "' in variable '" + receive.getVariable() + "'.");
                    return receive;
                }
            }

            if (startingActivity instanceof PickActivity) {
                pick = (PickActivity) startingActivity;
                OnMessage onMessage = getOnMessage(pick, operation);
                if (onMessage != null)
                    return pick;
            }
        }

        return null;
    }

    private OnMessage getOnMessage(PickActivity pick, QName operation) {
        List<OnMessage> onMessages = pick.getOnMessage();
        for (OnMessage message : onMessages) {
            if (operation.getLocalPart().equals(message.getOperation())) {
                return message;
            }
        }
        return null;
    }
    
    
    private void initializeCorrelations(InboundMessageActivity ima) throws CorrelationViolation, UninitializedVariable, InvalidExpressionValue, SubLanguageExecutionFault, SelectionFailure {
        InitiateEnumeration initiate;
        List<Correlation> correlations = ima.getCorrelations();

        if (correlations == null)
            return;

        for (Correlation correlation : correlations) {
            initiate = correlation.getInitiate();

            if (initiate != InitiateEnumeration.NO) {
                CorrelationSet set = ima.getContext().getCorrelationSet(correlation.getSet());

                if (initiate == InitiateEnumeration.JOIN && set.isInitiated()) {
                    set.initiate(true, ima.getVariable(), ima.getContext());
                }
                if (initiate == InitiateEnumeration.YES) {
                    set.initiate(false, ima.getVariable(), ima.getContext());
                }
            }
        }

    }

    private InboundMessageActivity findMatchingActivity(SOAPMessage request) {

        QName operation = getOperation(request);
        MessageVariable variable = asVariable(request);

        for (InboundMessageActivity ima : listeners) {
            if (operation.getLocalPart().equals(ima.getOperation())) {

                if (matchCorrelation(variable, ima))
                    return ima;

            }
        }

        return null;
    }

    private MessageVariable asVariable(SOAPMessage request) {
        try {
            QName operation = getOperation(request);
            String style = getStyle(operation);
            QName type = getMessageType(operation.getLocalPart());
            MessageVariable variable = new MessageVariable(type, "", this.schemas, this.services);

            MessageBinding.bind(request, variable, style, operation, MessageBinding.REQUEST);
            return variable;

        } catch (Exception exception) {
            exception.printStackTrace();
            return null;
        }
    }

    private boolean matchCorrelation(MessageVariable message, InboundMessageActivity ima) {
        try {
            InitiateEnumeration initiate;
            List<Correlation> correlations = ima.getCorrelations();
            if (correlations == null)
                return true;
            
            for (Correlation correlation : correlations) {
                initiate = correlation.getInitiate();

                if (initiate != InitiateEnumeration.YES) {

                    CorrelationSet set = ima.getContext().getCorrelationSet(correlation.getSet());
                    if (initiate == InitiateEnumeration.JOIN && set.isInitiated()) {
                        set.checkConsistency(message, ima.getContext());
                    }
                    if (initiate == InitiateEnumeration.NO) {
                        set.checkConsistency(message, ima.getContext());
                    }
                }
            }
        } catch (CorrelationViolation exception) {

            return false;

        } catch (Exception exception) {

            System.err.println("ERROR: Correlation check failed.");
            exception.printStackTrace();

        }

        return true;

    }

    private String getStyle(QName operation) {
        Binding binding = services.getBinding(portType.getQName());
        SOAPBinding soapBinding = (SOAPBinding) binding.getExtensibilityElements().get(0);
        if (soapBinding.getStyle() != null)
            return soapBinding.getStyle();
        else {
            SOAPOperation soapOperation = (SOAPOperation) binding.getBindingOperation(operation.getLocalPart(), null, null).getExtensibilityElements().get(0);
            return soapOperation.getStyle();
        }
    }

    private QName getMessageType(String operationName) {

        Operation operation = portType.getOperation(operationName, null, null);
        return operation.getInput().getMessage().getQName();

    }

    /**
     * Gets the name of the operation invoked by the request message.
     * <p>
     * If the operation is a RPC/literal style, then the operation name is the
     * local name of the soap body child element. If it is a Document/literal
     * operation we have to find an operation with an input message with only
     * one part of same element qname as the soap body child element.
     * <p>
     * As we do not know if the message is in RPC or Document style, we first
     * check to see if there is a matching RPC operation.
     * 
     * @param request
     * @return operation name
     */
    @SuppressWarnings("unchecked")
    private QName getOperation(SOAPMessage request) {
        Element element;
        QName elementQName;
        try {
            List<Operation> operations = portType.getOperations();
            Iterator iterator = request.getSOAPBody().getChildElements();
            if (iterator.hasNext()) {
                element = (Element) iterator.next();
                elementQName = new QName(element.getNamespaceURI(), element.getLocalName());

                // Look for a RPC style operation
                for (Operation operation : operations) {
                    if (operation.getName().equals(element.getLocalName()))
                        return elementQName;
                }

                // Look for Document style operation
                for (Operation operation : operations) {
                    Collection parts = operation.getInput().getMessage().getParts().values();
                    if (parts.size() == 1) {
                        Part part = (Part) parts.iterator().next();
                        if (elementQName.equals(part.getElementName()))
                            return new QName(operation.getName());
                    }
                }
            } else {
                // No child element of SOAPBody: This can only be a
                // Documnet/literal message without any parts.
                for (Operation operation : operations) {
                    Collection parts = operation.getInput().getMessage().getParts().values();
                    if (parts.size() == 0) {
                        return new QName(operation.getName());
                    }
                }

            }
            return null;
        } catch (SOAPException exception) {

            exception.printStackTrace();
            return null;
        }
    }

    /**
     * @param ima Inbound Message Activity
     * @return true if successfully added
     * @see java.util.List#add(java.lang.Object)
     */
    public boolean add(InboundMessageActivity ima) {
        return listeners.add(ima);
    }

    /**
     * @param ima Inbound Message Activity
     * @return true if successfully removed
     * @see java.util.List#remove(java.lang.Object)
     */
    public boolean remove(InboundMessageActivity ima) {
        return listeners.remove(ima);
    }

}
