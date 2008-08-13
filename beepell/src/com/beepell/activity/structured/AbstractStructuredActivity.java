package com.beepell.activity.structured;

import java.util.ArrayList;
import java.util.List;

import com.beepell.activity.AbstractActivity;
import com.beepell.activity.Activity;
import com.beepell.activity.ActivityFactory;
import com.beepell.activity.ActivityState;
import com.beepell.activity.StateChangeListener;
import com.beepell.execution.ExecutionContext;
import com.beepell.execution.ExecutionThread;
import com.beepell.linkgraph.Link;
import com.beepell.linkgraph.LinkGraph;
import com.beepell.model.FlowActivity;
import com.beepell.model.ScopeActivity;
import com.beepell.repository.SchemaRepository;

/**
 * @author Tim Hallwyl
 */
public abstract class AbstractStructuredActivity extends AbstractActivity {

    private final static ActivityFactory factory = new ActivityFactory();

    private final List<Activity> children = new ArrayList<Activity>();

    protected AbstractStructuredActivity(com.beepell.model.Activity configuration) {
        super(configuration);
    }

    /**
     * Adds dead link dependency rules to the flow context.
     * <p>
     * If a child activity is not executed, due to the semantics of this
     * activity all of it outgoing links must be set false, but not before all
     * incoming links in the dependency chain are set (disregarding their
     * value).
     * <p>
     * This method is not used by the AbstractStructuredActivity, but is
     * provided for the implementations of the structured activities.
     * 
     * @param configuration
     */
    protected final void setOutgoingLinksFalse(com.beepell.model.Activity configuration) {
        if (configuration == null)
            return;

        LinkGraph graph = new LinkGraph(configuration);
        List<Link> leavingLinks = graph.getLeavingLinks();
        for (Link link : leavingLinks) {
            context.addDeadLinkDependency(link, graph.getDependencies(link));
        }

    }
    
    public void terminate() {
        ActivityState state = getState();
        if (state != ActivityState.WAITING && state != ActivityState.RUNNING)
            return;

        setState(ActivityState.TERMINATING);
        
        for (Activity activity : children) {
            activity.terminate();
        }

        synchronized (this) {
            this.notifyAll();    
        }        
    }
    
    /**
     * Executes the activity described by the model configuration object in the current context.
     * 
     * @param configuration
     */
    protected final Activity execute(com.beepell.model.Activity configuration) {
        return execute(configuration, context);
    }

    /**
     * Executes the activity described by the model configuration object in the specified context.
     * 
     * @param context
     * @param configuration
     */
    protected final Activity execute(com.beepell.model.Activity configuration, ExecutionContext context) {
        Activity activity;
        
        if (configuration instanceof ScopeActivity)
            activity = factory.create((ScopeActivity) configuration, context.getScope());
        
        else if (configuration instanceof FlowActivity)
            activity = factory.create((FlowActivity) configuration, context.getFlow());
        
        else
            activity = factory.create(configuration);
        
        addChild(activity);
        activity.execute(context);
        return activity;
    }
    
    
    /**
     * Used in serial forEach.
     * @param configuration
     * @param context
     * @param counterVarialeName
     * @param counterVariableValue
     * @return the live activity.
     * @throws Exception
     */
    protected final Scope execute(ScopeActivity configuration, ExecutionContext context, String counterVarialeName, long counterVariableValue, SchemaRepository schemas) {
        Scope scope = factory.create(configuration, context.getScope(), counterVarialeName, counterVariableValue, schemas);
        addChild(scope);
        scope.execute(context);
        return scope;
    }

    /**
     * Execute the activity in a concurrent thread, in the specified context. Used by the Flow activity.
     * @param configuration
     * @param context
     * @return The running thread.
     * @throws Exception
     */
    protected final ExecutionThread executeThread(com.beepell.model.Activity configuration, ExecutionContext context) {
        Activity activity;
        
        if (configuration instanceof ScopeActivity)
            activity = factory.create((ScopeActivity) configuration, context.getScope());
        
        else if (configuration instanceof FlowActivity)
            activity = factory.create((FlowActivity) configuration, context.getFlow());
        
        else
            activity = factory.create(configuration);
        
        addChild(activity);
        ExecutionThread thread = new ExecutionThread(activity, context);
        thread.start();
        return thread;    
    }
    
    /**
     * Used by parallel ForEach to execute a scope with a counter vairable and get notified on completion.
     * @param configuration the scope activity configuration to execute.
     * @param context 
     * @param foreach (this ForEach activity) to notify when thread has finished.
     * @return The running thread.
     * @throws Exception
     */
    protected final ExecutionThread executeThread(ScopeActivity configuration, ExecutionContext context, ForEach forEach, String counterVarialeName, long counterVariableValue, SchemaRepository schemas) {
        Scope scope = factory.create(configuration, context.getScope(), counterVarialeName, counterVariableValue, schemas);
        addChild(scope);
        ExecutionThread thread = new ExecutionThread(scope, context, forEach);
        thread.start();
        return thread;
    }


    private void addChild(Activity child) {
        children.add(child);       
        for (StateChangeListener listener : listeners) {
            listener.addedChild(this, child);
            child.addListener(listener);
        }
    }
    
    /**
     * Gets a list of child activities, that have begun execution. This includes
     * failed, terminated, completed and still living activities.
     * 
     * @return a list of child activities.
     */
    public final List<Activity> getChildren() {
        return children;
    }
    
    /**
     * Blocks until all threads in the array has finished.
     * @param threads
     */
    protected final static void join(final Thread[] threads) {
        boolean interrupted;
        for (int i = 0; i < threads.length; i++) {
            do {
                try {
                    interrupted = false;
                    threads[i].join();
                } catch (InterruptedException exception) {
                    interrupted = true;
                }
            } while (interrupted);
        }
    }

}
