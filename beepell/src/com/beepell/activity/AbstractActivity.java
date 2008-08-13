package com.beepell.activity;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import com.beepell.exceptions.BPELFault;
import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.JoinFailure;
import com.beepell.exceptions.SubLanguageExecutionFault;
import com.beepell.execution.ExecutionContext;
import com.beepell.execution.LinkListener;
import com.beepell.model.Source;
import com.beepell.model.Target;
import com.beepell.model.Targets;

/**
 * Abstract implementation of an BPEL activity. This class provides common logic
 * and utilities for activities.
 * 
 * @author Tim Hallwyl
 */
public abstract class AbstractActivity implements Activity, LinkListener {

    protected Logger log = Logger.getLogger("com.beepell.execution");

    private ActivityState state = ActivityState.INITIALIZING;

    // Standard elements
    private final List<Source> sources;

    private final Targets targets;

    // Standard attributes
    private final String name;

    private final boolean suppressJoinFailure;

    // Activity configuration
    protected final com.beepell.model.Activity configuration;

    protected ExecutionContext context;

    protected final ArrayList<StateChangeListener> listeners = new ArrayList<StateChangeListener>();

    protected AbstractActivity(com.beepell.model.Activity configuration) {
        if (configuration == null)
            throw new IllegalArgumentException("Cannot configure activity with a null configuration.");

        this.configuration = configuration;

        this.name = configuration.getName();

        this.suppressJoinFailure = configuration.isSuppressJoinFailure();

        if (configuration.getSources() != null)
            sources = configuration.getSources().getSource();
        else
            sources = new ArrayList<Source>(0);

        if (configuration.getTargets() != null)
            targets = configuration.getTargets();
        else
            targets = null;
    }

    /**
     * Gets the name of the activity, or null if no name is specified.
     * 
     * @return the name of the activity
     */
    public String getName() {
        return this.name;
    }

    protected final void setState(ActivityState newState) {
        if (newState.equals(state))
            return;

        ActivityState oldState = state;
        this.state = newState;

        for (StateChangeListener listener : listeners) {
            listener.stateChanged(this, oldState, newState);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.activity.ActivityInterface#getState()
     */
    public final ActivityState getState() {
        return state;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.activity.ActivityInterface#addListener(com.beepell.activity.StateChangeListener)
     */
    public final void addListener(StateChangeListener listener) {
        listeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.activity.ActivityInterface#removeListner(com.beepell.activity.StateChangeListener)
     */
    public final void removeListner(StateChangeListener listener) {
        listeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.activity.ActivityInterface#execute()
     */
    public final synchronized void execute(ExecutionContext context) {
        if (state != ActivityState.READY)
            throw new IllegalStateException("Activity '" + getClass().getSimpleName() + "' is not in READY state.");

        this.context = context;
        this.log = Logger.getLogger("com.beepell.execution.instance." + context.getInstance().hashCode());

        this.context.getInstance().next(this);
        try {

            try {

                if (!evaluateJoinCondition()) {

                    /*
                     * When a target activity is not performed due to the value
                     * of the <joinCondition> (implicit or explicit) being
                     * false, its outgoing links MUST be assigned a false status
                     * according to the rules of section 11.6.2. [11.6.3]
                     */
                    setOutgoinLinksFalse();
                    throw new JoinFailure("Join condition evaluated false.");
                }

            } catch (JoinFailure joinFailure) {
                /*
                 * a standard bpel:joinFailure fault MUST be thrown, unless the
                 * value of suppressJoinFailure is yes in which case
                 * bpel:joinFailure is not thrown. [11.6.2]
                 */
                if (suppressJoinFailure) {
                    setState(ActivityState.SKIPPED);
                    return;
                } else {
                    throw joinFailure;
                }
            }

            // Activity might have been terminated while waiting for incoming
            // links and evaluating join condition.
            if (state != ActivityState.TERMINATING) {
                // The activity is enabled, start execution
                setState(ActivityState.RUNNING);
                run();
            }

            // Execution has stopped, either by forced termination or completion
            if (state == ActivityState.TERMINATING) {
                setState(ActivityState.TERMINATED);
                // TODO: Set links dead
                setOutgoinLinksFalse();
                return;
            }

            if (state == ActivityState.FAILING) {
                /* Scope specific state */
                setState(ActivityState.FAILED);
                /*
                 * When a fault handler for a scope completes handling a fault
                 * that occurred in that scope without throwing a fault itself,
                 * links that have that scope as the source MUST be subject to
                 * evaluation of their status. [12.5]
                 */
                doLinkTransitions();
                return;
            }

            if (state == ActivityState.RUNNING) {
                setState(ActivityState.COMPLETED);
                // When activity A completes without propagating any fault [...]
                // determine the status of all outgoing links for A. [11.6.2]
                doLinkTransitions();
            }

        } catch (BPELFault fault) {
            // Execution failed
            setState(ActivityState.FAILED);
            log.info(getClass().getSimpleName() + " activity has failed: (" + fault.getClass().getSimpleName() + ") " + fault.getLocalizedMessage());

            Throwable cause = fault.getCause();
            while (cause != null) {
                if (cause.getLocalizedMessage() != null)
                    log.info(" - caused by '" + cause.getLocalizedMessage() + "'.");
                cause = cause.getCause();
            }
            fault.printStackTrace();
            if (context.getScope() != null)
                context.getScope().fail(fault);
            
            setOutgoinLinksFalse();
        } catch (Exception exception) {
            log.severe(getClass().getSimpleName() + " activity has failed: (" + exception.getClass().getSimpleName() + ") " + exception.getLocalizedMessage());
            exception.printStackTrace();
        }

    }

    /**
     * This is the method to be implemented by the activity implementations.
     * 
     * @throws Exception
     */
    protected abstract void run() throws BPELFault;

    /**
     * <p>
     * If an activity that is ready to start [...] has incoming links, then it
     * MUST NOT start until the status of all its incoming links has been
     * determined and the, implicit or explicit, join condition has been
     * evaluated. In order to avoid violating control dependencies, evaluation
     * of the join condition is performed only after the status of all incoming
     * links has been determined.
     * <p>
     * This method waits for incoming links, before evaluating the join
     * condition. <b>Note:</b> This method returns false, even if suppress join
     * failure is false. If suppressJoinFailure also is false, then this should
     * cause a join (failure) exception, but this is NOT handled in this method.
     * <p>
     * An exception is thrown if the join condition could not be evaluated.
     * 
     * @throws JoinFailure
     * @return true if the join condition evaluated to true, but only false if
     *         the join condition evaluated to false AND suppress join failure
     *         is false. Otherwise an join (failure) exception is thrown.
     */
    private boolean evaluateJoinCondition() throws JoinFailure {
        if (this.targets == null)
            return true;

        setState(ActivityState.WAITING);

        try {
            List<Target> targets = this.targets.getTarget();

            while (!areIncommingLinksDetermined()) {
                context.addLinkListener(this, this.targets.getTarget());
                wait();
                if (getState() == ActivityState.TERMINATING)
                    return false;
            }

            if (this.targets.getJoinCondition() == null) {
                // If no <joinCondition> is specified, the <joinCondition> is
                // the disjunction (i.e. a logical OR operation) of the link
                // status of all incoming links of this activity. [11.6.1]

                for (Target target : targets) {
                    if (context.getLinkState(target.getLinkName()) == true)
                        return true;
                }
                return false;

            } else {
                // A join condition is specified and evaluated.

                return this.targets.getJoinCondition().evaluate(context);
            }

        } catch (Exception exception) {
            if (this.suppressJoinFailure)
                return false;
            else
                throw new JoinFailure("Failed to evaluate the join condition in '" + this.name + "'.", exception);
        }
    }

    private boolean areIncommingLinksDetermined() {

        List<Target> targets = this.targets.getTarget();
        for (Target target : targets) {
            if (context.getLinkState(target.getLinkName()) == null)
                return false;
        }
        return true;
    }

    /**
     * Determine the status of all outgoing links.
     * <p>
     * Each &lt;source&gt; element can specify an optional
     * &lt;transitionCondition&gt; as a guard for following the specified link.
     * If the &lt;transitionCondition&gt; is omitted, it is assumed to evaluate
     * to true.
     * <p>
     * When an activity has multiple outgoing links, the order in which the
     * status of the links and the associated transition conditions are
     * evaluated is defined to be sequential, according to the order the links
     * are declared in the &lt;source&gt; element.
     * <p>
     * If an error occurs while evaluating the &lt;transitionCondition&gt;, that
     * error does not affect the completion status of the activity and is
     * handled by the source activity's enclosing scope.
     */
    private void doLinkTransitions() throws SubLanguageExecutionFault, InvalidExpressionValue {
        try {

            for (Source source : sources) {
                if (source.getTransitionCondition() != null) {
                    context.setLinkState(source.getLinkName(), source.getTransitionCondition().evaluate(context));

                } else {
                    context.setLinkState(source.getLinkName(), true);
                }
            }

        } catch (SubLanguageExecutionFault exception) {
            failedTransition();
            throw exception;
        } catch (InvalidExpressionValue exception) {
            failedTransition();
            throw exception;
        } catch (Exception exception) {
            failedTransition();
            throw new SubLanguageExecutionFault(exception);
        }
    }

    /**
     * <p>
     * If an error occurs while evaluating the transition condition of one of an
     * activity's outgoing links, then all remaining outgoing links with targets
     * within the source activity's enclosing scope MUST NOT have their
     * transition conditions evaluated and remain in the unset state. However,
     * if the target of a remaining outgoing link is outside the source
     * activity's enclosing scope then the status of the link MUST be set to
     * false.
     */
    private void failedTransition() {
        for (Source source : sources) {
            if (context.getLinkState(source.getLinkName()) == null && context.isInterScopeLink(configuration, source.getLinkName()))
                context.setLinkState(source.getLinkName(), false);
        }
    }

    /**
     * If the join condition fails, then all outgoing links must be set false.
     * 
     * @param configuration
     */
    private void setOutgoinLinksFalse() {
        for (Source source : sources)
            context.setLinkState(source.getLinkName(), false);
    }

    public synchronized void linkChanged(String linkName) {
        this.notifyAll();
    }

    /*
     * If the activity is already in an ended state, terminate has no effect.
     * 
     * @see com.beepell.activity.ActivityInterface#terminate()
     */
    public void terminate() {
        ActivityState state = getState();
        if (state == ActivityState.COMPLETED)
            return;
        if (state == ActivityState.FAILED)
            return;
        if (state == ActivityState.TERMINATED)
            return;

        setState(ActivityState.TERMINATING);

        synchronized (this) {
            this.notifyAll();
        }

    }

}
