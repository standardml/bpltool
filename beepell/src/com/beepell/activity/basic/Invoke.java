package com.beepell.activity.basic;

import java.util.List;
import java.util.concurrent.Future;

import com.beepell.activity.ActivityState;
import com.beepell.broker.MessageBroker;
import com.beepell.exceptions.BPELFault;
import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.UninitializedPartnerRole;
import com.beepell.model.CorrelationWithPattern;
import com.beepell.model.InvokeActivity;

/**
 * <p>
 * The invoke activity is used to call Web Services offered by service providers
 * ... The typical use is invoking an operation on a service, which is
 * considered a basic activity. ... Operations can be request-response or
 * one-way operations, corresponding to WSDL 1.1 operation definitions. [10.3]
 * 
 * @author Tim Hallwyl
 */
public class Invoke extends AbstractBasicActivity {

    private final String inputVariable;

    private final String outputVariable;

    private final String operation;

    private final String partnerLink;

    private final List<CorrelationWithPattern> correlations;

    private boolean complete = false;

    private boolean failed = false;

    private BPELFault fault = null;

    /**
     * @param configuration
     */
    public Invoke(InvokeActivity configuration) {
        super(configuration);

        this.inputVariable = configuration.getInputVariable();
        this.outputVariable = configuration.getOutputVariable();
        this.operation = configuration.getOperation();
        this.partnerLink = configuration.getPartnerLink();
        
        if (configuration.getCorrelations() != null)
            this.correlations = configuration.getCorrelations().getCorrelation();
        else
            this.correlations = null;
        
        setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() throws UninitializedPartnerRole, InvalidVariables, BPELFault {

        /*
         * Before invoking the partner link, we validate the message to be sent.
         * Note, if a WSDL message definition does not contain any parts, then
         * the inputVariable MAY be omitted, in which case there is nothing to
         * validate.
         */
        if (inputVariable != null)
            context.validate(inputVariable);

        /*
         * If an invoke activity is used on a partnerLink whose partnerRole EPR
         * is not initialized then a bpel:uninitializedPartnerRole fault MUST be
         * thrown. [10.3]
         */
        if (!context.isPartnerRoleInitialized(partnerLink))
            throw new UninitializedPartnerRole("PartnerLink '" + partnerLink + "' is not initialized.");



        try {
            Future<?> future = MessageBroker.invoke(this, context);
            while (!complete) {
                wait();
                if (getState() == ActivityState.TERMINATING) {
                    future.cancel(true);
                    return;
                }
                if (failed) {
                    throw fault;
                }
            }
        } catch (InterruptedException exception) {
            return;
        }

        /*
         * If it is a request-response invocation, we validate the response
         * received.
         */

        if (outputVariable != null)
            context.validate(outputVariable);

    }
    
    /**
     * Call-back for MessageExchange to notify that either a message was received
     * in reply and is in the output variable or, if a one-way operation, that
     * the operation is complete.
     */
    public synchronized void messageReceived() {
        this.complete = true;
        this.notifyAll();
    }

    /**
     * Call-back for MessageExchage to set fault.
     * 
     * @param fault caused by a fault message, correlation violation, or protocol error.
     */
    public synchronized void failed(BPELFault fault) {
        this.failed = true;
        this.fault = fault;
        this.notifyAll();
    }

    
    /**
     * @return the correlations
     */
    public List<CorrelationWithPattern> getCorrelations() {
        return correlations;
    }

    
    /**
     * @return the inputVariable
     */
    public String getInputVariable() {
        return inputVariable;
    }

    
    /**
     * @return the operation
     */
    public String getOperation() {
        return operation;
    }

    
    /**
     * @return the outputVariable
     */
    public String getOutputVariable() {
        return outputVariable;
    }

    
    /**
     * @return the partnerLink
     */
    public String getPartnerLink() {
        return partnerLink;
    }

}
