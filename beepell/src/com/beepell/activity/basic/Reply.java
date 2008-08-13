package com.beepell.activity.basic;

import java.util.List;

import javax.xml.namespace.QName;

import com.beepell.activity.ActivityState;
import com.beepell.exceptions.BPELFault;
import com.beepell.execution.ExecutionContext;
import com.beepell.model.Correlation;
import com.beepell.model.ReplyActivity;

/**
 * Reply Activity.
 * <p>
 * A business process provides services to its partners through inbound message
 * activities and corresponding reply activities.
 * 
 * @author Tim Hallwyl
 */
public class Reply extends AbstractBasicActivity {

    private final List<Correlation> correlations;

    private final QName faultName;

    private final String messageExchange;

    private final String operation;

    private final String partnerLink;

    private final String variable;

    private boolean complete = false;

    private BPELFault fault = null;

    /**
     * Create a Reply Activity.
     * 
     * @param configuration
     */
    public Reply(ReplyActivity configuration) {
        super(configuration);

        if (configuration.getCorrelations() != null)
            correlations = configuration.getCorrelations().getCorrelation();
        else
            correlations = null;
        this.faultName = configuration.getFaultName();
        this.messageExchange = configuration.getMessageExchange();
        this.operation = configuration.getOperation();
        this.partnerLink = configuration.getPartnerLink();
        this.variable = configuration.getVariable();
        this.setState(ActivityState.READY);

    }

    @Override
    protected synchronized void run() throws BPELFault {

        InboundMessageActivity ima = context.getOpenIMA(this.partnerLink, this.operation, this.messageExchange);
        ima.reply(this);

        try {
            while (complete == false) {
                wait();

                if (fault != null)
                    throw fault;

                if (getState() == ActivityState.TERMINATING)
                    return;

            }
        } catch (InterruptedException exception) {
            return;
        }

    }

    /**
     * @return the correlations
     */
    public List<Correlation> getCorrelations() {
        return correlations;
    }

    /**
     * @return the faultName
     */
    public QName getFaultName() {
        return faultName;
    }

    /**
     * @return the messageExchange
     */
    public String getMessageExchange() {
        return messageExchange;
    }

    /**
     * @return the operation
     */
    public String getOperation() {
        return operation;
    }

    /**
     * @return the partnerLink
     */
    public String getPartnerLink() {
        return partnerLink;
    }

    /**
     * @return the variable
     */
    public String getVariable() {
        return variable;
    }

    /**
     * @return the execution context.
     */
    public ExecutionContext getContext() {
        return context;
    }

    /**
     * Call-back for MessageBroker (actually InboundMessageHandler) to nofity on
     * completion.
     */
    public synchronized void complete() {
        this.complete = true;
        this.notifyAll();
    }

    /**
     * Call-back for MessageBroker (actually InboundMessageHandler) to set fault.
     * 
     * @param fault caused by a fault message, correlation violation, or
     *            protocol error.
     */
    public synchronized void failed(BPELFault fault) {
        this.fault = fault;
        this.notifyAll();
    }

}
