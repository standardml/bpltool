package com.beepell.activity.basic;

import java.util.List;

import javax.wsdl.PortType;

import com.beepell.activity.ActivityState;
import com.beepell.broker.MessageBroker;
import com.beepell.exceptions.BPELFault;
import com.beepell.execution.ExecutionContext;
import com.beepell.execution.PartnerLink;
import com.beepell.execution.ProcessInstanceState;
import com.beepell.model.Correlation;
import com.beepell.model.ReceiveActivity;
import com.beepell.repository.PartnerLinkType;

/**
 * <p>
 * A receive activity specifies the partnerLink that contains the myRole used to
 * receive messages and operation that it expects the partner to invoke. The
 * value of the partnerRole in the partnerLink is not used when processing a
 * receive activity.
 * <p>
 * In addition, receive specifies a variable, using the variable attribute, that
 * is to be used to receive the message data. If a WSDL message definition does
 * not contain any parts, then the associated variable attribute MAY be omitted
 * 
 * @author Tim Hallwyl
 */
public class Receive extends AbstractBasicActivity implements InboundMessageActivity {

    private final String variable;

    private final String operation;

    private final String partnerLink;

    private final List<Correlation> correlations;

    private final String messageExchange;

    private boolean complete = false;

    private boolean failed = false;

    private BPELFault fault = null;
    
    /**
     * The reply activity matching this receive. This is not set until the reply is reached.
     */
    private Reply reply;

    /**
     * @param configuration
     */
    public Receive(ReceiveActivity configuration) {
        super(configuration);
        this.variable = configuration.getVariable();
        this.operation = configuration.getOperation();
        this.partnerLink = configuration.getPartnerLink();
        if (configuration.getCorrelations() != null)
            this.correlations = configuration.getCorrelations().getCorrelation();
        else
            this.correlations = null;

        this.messageExchange = configuration.getMessageExchange();

        setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() throws BPELFault {
        boolean isInitialStartingActivity = context.isInitialStartingActivity(this.configuration);

        /*
         * When the process is in STARTING state, other starting activities must
         * wait until the initial starting activity has completed.
         */
        if (!isInitialStartingActivity && context.getInstanceState() == ProcessInstanceState.STARTING) {
            try {
                context.notifyOnRunningState(this);
                while (context.getInstanceState() == ProcessInstanceState.STARTING) {
                    wait();
                }
            } catch (InterruptedException exception) {
                return;
            }
        }

        MessageBroker.receive(this);

        if (isInitialStartingActivity)
            context.setInitialStartingActivityReady(this);

        try {
            while (!complete) {
                wait();
                if (getState() == ActivityState.TERMINATING) {
                    MessageBroker.cancel(this);
                    return;
                }
                if (failed)
                    throw fault;
            }
        } catch (InterruptedException exception) {
            return;
        }

        if (this.variable != null)
            context.validate(this.variable);

        /*
         * Make the process instance aware that the initial starting activity
         * has completed, so other starting activities may execute.
         */
        if (isInitialStartingActivity)
            context.setInitialStartingActivityComplete(this);

    }

    /**
     * @return the variable
     */
    public String getVariable() {
        return variable;
    }

    /**
     * @return the operation
     */
    public String getOperation() {
        return operation;
    }

    /**
     * @return the correlations
     */
    public List<Correlation> getCorrelations() {
        return correlations;
    }

    /**
     * @return the partnerLink
     */
    public String getPartnerLink() {
        return partnerLink;
    }

    /**
     * @return the execution context.
     */
    public ExecutionContext getContext() {
        return context;
    }

    /**
     * @return the messageExchange
     */
    public String getMessageExchange() {
        return messageExchange;
    }

    /**
     * 
     */
    public synchronized Reply messageReceived() {
        this.complete = true;
        this.notifyAll();
        
        if (isOneWay())
            return null;
        else {
            /*
             * A request-response operation, we must wait for a reply activity to set the reply.
             */
            context.addOpenIMA(this);
            try {
                while (this.reply == null) {
                    wait();                    
                }
                
                return reply;
            } catch (InterruptedException exception) {
                return null;
            }
            
            
        }           
    }
    
    /**
     * Used by the matching Reply activity to send the reply.
     * @param reply
     */
    public synchronized void reply(Reply reply) {
        if (this.reply != null)
            throw new IllegalStateException("A reply has already been sent on this receive.");
        
        this.reply = reply;
        this.context.removeOpenIMA(this);
        this.notifyAll();
    }

    /**
     * This method returns true if the operation exposed by this receive is a
     * one-way operation, false if it is a request-respose.
     * 
     * @return true if the operation exposed by this receive is a one-way
     *         operation, false if it is a request-respose.
     */
    private boolean isOneWay() {
        PartnerLink partnerLink = context.getPartnerLink(this.partnerLink);
        PartnerLinkType partnerLinkType = context.getServiceRepository().getPartnerLinkType(partnerLink.getPartnerLinkType());
        PortType portType = partnerLinkType.getPortType(partnerLink.getMyRole());
        if (portType.getOperation(this.operation, null, null).getOutput() == null)
            return true;
        else
            return false;

    }

    /**
     * 
     */
    public synchronized void failed(BPELFault fault) {
        this.failed = true;
        this.fault = fault;
        this.notifyAll();
    }
}
