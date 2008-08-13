package com.beepell.activity;

import java.util.logging.Logger;

import com.beepell.activity.basic.Assign;
import com.beepell.activity.basic.Compensate;
import com.beepell.activity.basic.CompensateScope;
import com.beepell.activity.basic.Empty;
import com.beepell.activity.basic.Exit;
import com.beepell.activity.basic.Invoke;
import com.beepell.activity.basic.Receive;
import com.beepell.activity.basic.Reply;
import com.beepell.activity.basic.Rethrow;
import com.beepell.activity.basic.Throw;
import com.beepell.activity.basic.Validate;
import com.beepell.activity.basic.Wait;
import com.beepell.activity.structured.Flow;
import com.beepell.activity.structured.ForEach;
import com.beepell.activity.structured.If;
import com.beepell.activity.structured.Pick;
import com.beepell.activity.structured.RepeatUntil;
import com.beepell.activity.structured.Scope;
import com.beepell.activity.structured.Sequence;
import com.beepell.activity.structured.While;
import com.beepell.model.AssignActivity;
import com.beepell.model.CompensateActivity;
import com.beepell.model.CompensateScopeActivity;
import com.beepell.model.EmptyActivity;
import com.beepell.model.ExitActivity;
import com.beepell.model.FlowActivity;
import com.beepell.model.ForEachActivity;
import com.beepell.model.IfActivity;
import com.beepell.model.InvokeActivity;
import com.beepell.model.PickActivity;
import com.beepell.model.ProcessDescription;
import com.beepell.model.ReceiveActivity;
import com.beepell.model.RepeatUntilActivity;
import com.beepell.model.ReplyActivity;
import com.beepell.model.RethrowActivity;
import com.beepell.model.ScopeActivity;
import com.beepell.model.SequenceActivity;
import com.beepell.model.ThrowActivity;
import com.beepell.model.ValidateActivity;
import com.beepell.model.WaitActivity;
import com.beepell.model.WhileActivity;
import com.beepell.repository.SchemaRepository;

/**
 * Factory to encapsulate creation of concrete executable activities.
 * 
 * @author Tim Hallwyl
 */
public final class ActivityFactory {

    private Logger log = Logger.getLogger("com.beepell.execution");
    
    /**
     * Creates an Activity based on a configuration.
     * 
     * @param configuration the configuration to create an activity.
     * @return a new configured activity object.
     */
    public Activity create(com.beepell.model.Activity configuration) {
        if (configuration == null)
            throw new IllegalArgumentException("Cannot use null as configuration.");
        
        /* BASIC ACTIVITIES */
        if (configuration instanceof AssignActivity)
            return new Assign((AssignActivity) configuration);
        
        if (configuration instanceof CompensateActivity)
            return new Compensate((CompensateActivity) configuration);      

        if (configuration instanceof CompensateScopeActivity)
            return new CompensateScope((CompensateScopeActivity) configuration);  
        
        if (configuration instanceof EmptyActivity)
            return new Empty((EmptyActivity) configuration);

        if (configuration instanceof ExitActivity)
            return new Exit((ExitActivity) configuration);

        if (configuration instanceof InvokeActivity)
            return new Invoke((InvokeActivity) configuration);
        
        if (configuration instanceof ReceiveActivity)
            return new Receive((ReceiveActivity) configuration);
        
        if (configuration instanceof ReplyActivity)
            return new Reply((ReplyActivity) configuration);

        if (configuration instanceof RethrowActivity)
            return new Rethrow((RethrowActivity) configuration);

        if (configuration instanceof ThrowActivity)
            return new Throw((ThrowActivity) configuration);

        if (configuration instanceof ValidateActivity)
            return new Validate((ValidateActivity) configuration);

        if (configuration instanceof WaitActivity)
            return new Wait((WaitActivity) configuration);

        /* STRUCTURED ACTIVITIES */
        if (configuration instanceof FlowActivity)
            return new Flow((FlowActivity) configuration);

        if (configuration instanceof ForEachActivity)
            return new ForEach((ForEachActivity) configuration);

        if (configuration instanceof IfActivity)
            return new If((IfActivity) configuration);

        if (configuration instanceof PickActivity)
            return new Pick((PickActivity) configuration);

        if (configuration instanceof RepeatUntilActivity)
            return new RepeatUntil((RepeatUntilActivity) configuration);

        if (configuration instanceof ScopeActivity)
            return new Scope((ScopeActivity) configuration);

        if (configuration instanceof SequenceActivity)
            return new Sequence((SequenceActivity) configuration);

        if (configuration instanceof WhileActivity)
            return new While((WhileActivity) configuration);

        log.severe("Activity factory does not know how to create from " + configuration.getClass().getSimpleName());
        return null;

    }

    /**
     * Creates a Scope activity with a counter variable, used in the ForEeach
     * activity.
     * 
     * @param configuration the configuration to create an activity.
     * @param parent the parent Scope
     * @param counterVariableName name of the counter variable
     * @param counterVariableValue value of the counter variable
     * @param schemas 
     * @return A Scope Activity with a counter variable.
     */
    public Scope create(ScopeActivity configuration, Scope parent, String counterVariableName, long counterVariableValue, SchemaRepository schemas) {
        return new Scope(configuration, parent, counterVariableName, counterVariableValue, schemas);
    }

    /**
     * Creates a Scope activity.
     * 
     * @param configuration the configuration to create an activity.
     * @param parent the parent Scope
     * @return A Scope Activity.
     */
    public Scope create(ScopeActivity configuration, Scope parent) {
        return new Scope(configuration, parent);
    }

    /**
     * Creates a global Scope activity for the process element.
     * 
     * @param configuration the configuration to create an activity.
     * @param parent the parent Scope
     * @return A Scope Activity.
     */
    public Scope create(ProcessDescription configuration) {
        ScopeActivity process = new ScopeActivity();

        process.setName(configuration.getName());
        process.setSuppressJoinFailure(configuration.isSuppressJoinFailure());
        process.setExitOnStandardFault(configuration.isExitOnStandardFault());

        process.setPartnerLinks(configuration.getPartnerLinks());
        process.setMessageExchanges(configuration.getMessageExchanges());
        process.setVariables(configuration.getVariables());
        process.setCorrelationSets(configuration.getCorrelationSets());
        process.setFaultHandlers(configuration.getFaultHandlers());
        process.setEventHandlers(configuration.getEventHandlers());
        process.setActivity(configuration.getActivity());

        return new Scope(process);
    }
    
    /**
     * Creates a Flow activity with a parent.
     * 
     * @param configuration the configuration to create an activity.
     * @param parent the parent Flow
     * @return A Flow Activity.
     */
    public Flow create(FlowActivity configuration, Flow parent) {
        return new Flow(configuration, parent);
    }
}
