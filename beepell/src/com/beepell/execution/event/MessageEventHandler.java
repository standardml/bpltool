package com.beepell.execution.event;

import java.util.ArrayList;
import java.util.List;

import com.beepell.activity.basic.InboundMessageActivity;
import com.beepell.activity.basic.Receive;
import com.beepell.activity.basic.Reply;
import com.beepell.activity.structured.Scope;
import com.beepell.activity.structured.Sequence;
import com.beepell.exceptions.BPELFault;
import com.beepell.execution.ExecutionContext;
import com.beepell.execution.ExecutionThread;
import com.beepell.model.Correlation;
import com.beepell.model.OnEvent;
import com.beepell.model.ReceiveActivity;
import com.beepell.model.ScopeActivity;
import com.beepell.model.SequenceActivity;
import com.beepell.model.Variable;
import com.beepell.model.Variables;

/**
 * Handler to register with message broker and handle instance creation of
 * events.
 * 
 * @author Tim Hallwyl
 */
public class MessageEventHandler implements InboundMessageActivity {

    private final ExecutionContext context;

    private final OnEvent configuration;

    private final ScopeActivity scope;

    /**
     * Create an message event handler.
     * 
     * @param configuration
     * @param context
     */
    public MessageEventHandler(OnEvent configuration, ExecutionContext context) {

        this.context = context;
        this.configuration = configuration;

        this.scope = rewrite(configuration);

    }

    /**
     * Re-write the onEvent into an executable Scope with a receive.
     * 
     * @param configuration
     * @return an executable Scope
     */
    private ScopeActivity rewrite(OnEvent configuration) {

        // TODO: This modifies the original configuration!
        // TODO: This does not support fromParts and element variables
        ScopeActivity scope = configuration.getScope();
        Variable variable = new Variable();
        variable.setName(configuration.getVariable());
        variable.setMessageType(configuration.getMessageType());
        scope.setVariables(new Variables());
        scope.getVariables().getVariable().add(variable);
        
        SequenceActivity sequence = new SequenceActivity();
        sequence.setSuppressJoinFailure(scope.isSuppressJoinFailure());
        
        ReceiveActivity receive = new ReceiveActivity();
        receive.setSuppressJoinFailure(scope.isSuppressJoinFailure());
        receive.setCorrelations(configuration.getCorrelations());
        receive.setCreateInstance(false);
        receive.setMessageExchange(configuration.getMessageExchange());
        receive.setOperation(configuration.getOperation());
        receive.setPartnerLink(configuration.getPartnerLink());
        receive.setVariable(configuration.getVariable());

        sequence.getActivity().add(receive);
        sequence.getActivity().add(scope.getActivity());
        scope.setActivity(sequence);

        return scope;
    }

    /**
     * Used by message broker to notified that a message has been received for
     * this event handler and is awaiting a receive activity. This handler will
     * then create a instance of the event.
     * @return the conceptual receive activity.
     */
    public InboundMessageActivity messageEnqueued() {
        ExecutionThread thread = context.getScope().executeMessageEvent(scope);        
        try { Thread.sleep(100); } catch (InterruptedException exception) { /* ignore */ }
        return ((Receive) ((Sequence) ((Scope) thread.getActivity()).getChildren().get(0)).getChildren().get(0));
    }

    public void failed(BPELFault exception) {
        throw new IllegalAccessError("Fault must be sent to the event instance");
    }

    public ExecutionContext getContext() {
        return context;
    }

    public List<Correlation> getCorrelations() {
        if (configuration.getCorrelations() == null)
            return null;

        /*
         * TODO: Is this necessary? If only sets with initialize "no" is
         * considered, do we then need to sort out undeclared sets?
         */
        List<Correlation> sets = new ArrayList<Correlation>();
        for (Correlation correlation : getCorrelations()) {
            try {
                context.getCorrelationSet(correlation.getSet());
                sets.add(correlation);
            } catch (IllegalArgumentException exception) {
                /* the correlation set was not found in parent scope */
            }
        }

        return sets;
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
        throw new IllegalAccessError("Variable is only in the context of the event instance.");
    }

    public Reply messageReceived() {
        throw new IllegalAccessError("Message be sent to the event instance.");
    }

    public void reply(Reply reply) {
        throw new IllegalAccessError("Reply must be sent to the event instance");
    }

}
