package com.beepell.activity.structured;

import java.util.List;
import java.util.logging.Logger;

import javax.wsdl.PortType;

import com.beepell.activity.basic.InboundMessageActivity;
import com.beepell.activity.basic.Reply;
import com.beepell.broker.MessageBroker;
import com.beepell.exceptions.BPELFault;
import com.beepell.execution.ExecutionContext;
import com.beepell.execution.PartnerLink;
import com.beepell.model.Correlation;
import com.beepell.model.OnMessage;
import com.beepell.repository.PartnerLinkType;

/**
 * Handler to handle OnMessage for Pick
 * @author Tim Hallwyl
 *
 */
public class OnMessageHandler implements InboundMessageActivity {

    private final OnMessage configuration;

    private final Pick pick;

    private final ExecutionContext context;

    private Reply reply;

    protected Logger log = Logger.getLogger("com.beepell.execution");
    
    /**
     * Create a handler to handle OnMessage for Pick
     * @param configuration
     * @param pick
     */
    public OnMessageHandler(OnMessage configuration, Pick pick) {
        this.configuration = configuration;
        this.pick = pick;
        this.context = pick.getContext();
        MessageBroker.receive(this);
        this.log = Logger.getLogger("com.beepell.execution.instance." + context.getInstance().hashCode());
    }

    public void failed(BPELFault exception) {
        log.warning("Receiving a message for Pick Activity caused a failure, which is ignored");
        MessageBroker.receive(this);
    }

    public ExecutionContext getContext() {
        return context;
    }

    public List<Correlation> getCorrelations() {
        if (configuration.getCorrelations() == null)
            return null;
        else
            return configuration.getCorrelations().getCorrelation();
    }

    public String getMessageExchange() {
        return configuration.getMessageExchange();
    }

    public String getOperation() {
        return configuration.getOperation();
    }

    public String getPartnerLink() {
        return configuration.getPartnerLink();
    }

    public String getVariable() {
        return configuration.getVariable();
    }

    public synchronized Reply messageReceived() {
        log.info("A message was message received for operation '" + this.getOperation() + "'.");
        if (isOneWay()) {
            pick.picked(this);
            return null;
        } else {
            /*
             * A request-response operation, we must wait for a reply activity
             * to set the reply.
             */
            context.addOpenIMA(this);
            if (!pick.picked(this)) {
                /* An other message raced in before this */
                context.removeOpenIMA(this);
                return null;
            }
            log.info("Waiting for reply on " + this.getOperation());
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

    public synchronized void reply(Reply reply) {

        if (this.reply != null)
            throw new IllegalStateException("A reply has already been sent on this OnMessage.");

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
        PartnerLink partnerLink = context.getPartnerLink(configuration.getPartnerLink());
        PartnerLinkType partnerLinkType = context.getServiceRepository().getPartnerLinkType(partnerLink.getPartnerLinkType());
        PortType portType = partnerLinkType.getPortType(partnerLink.getMyRole());
        if (portType.getOperation(this.configuration.getOperation(), null, null).getOutput() == null)
            return true;
        else
            return false;

    }


    /**
     * Gets the activity.
     * @return the activity.
     */
    public com.beepell.model.Activity getActivity() {
        return configuration.getActivity();
    }

    /**
     * Cancel waiting for a message. Used on termination of Pick and when Pick
     * has selected an other message or alarm.
     */
    public void cancel() {
        MessageBroker.cancel(this);
    }

}
