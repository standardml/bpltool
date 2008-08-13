package com.beepell.activity.basic;

import java.util.List;

import com.beepell.exceptions.BPELFault;
import com.beepell.execution.ExecutionContext;
import com.beepell.model.Correlation;

/**
 * Common interface for inbound message activities: Receive, Pick, OnEvent.
 * 
 * @author Tim Hallwyl
 */
public interface InboundMessageActivity {

    /**
     * Gets the list of correlations.
     * 
     * @return the list of correlations
     */
    public List<Correlation> getCorrelations();

    /**
     * Gets the message exchange name.
     * 
     * @return the message exchange name.
     */
    public String getMessageExchange();

    /**
     * Gets the variable name.
     * 
     * @return the variable name.
     */
    public String getVariable();

    /**
     * Gets the operation name.
     * 
     * @return the operation name.
     */
    public String getOperation();

    /**
     * Gets the partner link name.
     * 
     * @return the partner link name.
     */
    public String getPartnerLink();

    /**
     * Gets the execution context.
     * 
     * @return the execution context.
     */
    public ExecutionContext getContext();

    /**
     * Call-back method to notify that a message has been received and placed in
     * the specified variable.
     * 
     * @return matching Reply or null if it is a one-way operation.
     */
    public Reply messageReceived();

    /**
     * Call-back method to notify that a fault occurred.
     * 
     * @param exception the fault.
     */
    public void failed(BPELFault exception);

    
    /**
     * Used by the matching Reply activity to send the reply.
     * @param reply
     */
    public void reply(Reply reply);
}
