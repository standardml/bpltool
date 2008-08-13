package com.beepell.execution;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.beepell.Settings;
import com.beepell.activity.ActivityFactory;
import com.beepell.activity.ActivityState;
import com.beepell.activity.StateChangeListener;
import com.beepell.activity.basic.InboundMessageActivity;
import com.beepell.activity.structured.Scope;
import com.beepell.deployment.ProcessContext;
import com.beepell.model.Activity;
import com.beepell.model.OnMessage;
import com.beepell.model.PickActivity;
import com.beepell.model.ReceiveActivity;

/**
 * A process instance.
 * 
 * @author Tim Hallwyl
 */
public class ProcessInstance implements StateChangeListener {

    private static final ActivityFactory factory = new ActivityFactory();

    private ProcessInstanceState state = ProcessInstanceState.INITIALIZING;

    private final Activity initialStartingActivityConfiguration;

    private final List<Object> notifyOnRunningStateList = new ArrayList<Object>();

    private InboundMessageActivity initialStartingActivity = null;

    private final ProcessContext processContext;

    private final LinkedList<com.beepell.activity.Activity> waiting = new LinkedList<com.beepell.activity.Activity>();

    private int step = 0;

    private boolean run = false;

    private OnMessage initialOnMessage;

    protected final ArrayList<ProcessInstanceListener> listeners = new ArrayList<ProcessInstanceListener>();

    protected final ArrayList<StateChangeListener> activityListeners = new ArrayList<StateChangeListener>();

    private final Settings settings = Settings.getInstance();

    private final Logger logger;

    /**
     * The process element is constructed as a scope element containing the main
     * element.
     */
    private final Scope processScope;

    /**
     * The thread executing the process scope.
     */
    private Thread mainThread;

    private ProcessInstance(ProcessContext processContext, Activity initialStartingActivity) {
        this.processContext = processContext;
        this.initialStartingActivityConfiguration = initialStartingActivity;
        this.processScope = factory.create(processContext.getDescription());

        if (settings.getSetting("tools.tracker.enabled", "false").equals("false"))
            this.run = true;

        if (settings.getSetting("tools.tracker.mode", "step").equals("run"))
            this.run = true;

        this.processContext.addInstance(this);
        this.processScope.addListener(this);

        this.logger = Logger.getLogger("com.beepell.execution.instance." + this.hashCode());

    }

    /**
     * Create a process instance on a Receive activity.
     * 
     * @param processContext the process to instantiate
     * @param initialStartingActivity the activity that instantiates the process
     */
    public ProcessInstance(ProcessContext processContext, ReceiveActivity initialStartingActivity) {
        this(processContext, (Activity) initialStartingActivity);
    }

    /**
     * Create a process instance on a Pick activity.
     * 
     * @param processContext
     * @param initialStartingActivity
     * @param onMessage
     */
    public ProcessInstance(ProcessContext processContext, PickActivity initialStartingActivity, OnMessage onMessage) {
        this(processContext, initialStartingActivity);
        this.initialOnMessage = onMessage;
    }

    /**
     * Start executing the process instance.
     * 
     * @return the initial starting activity
     */
    public synchronized InboundMessageActivity start() {
        if (state != ProcessInstanceState.INITIALIZING)
            throw new IllegalStateException("Cannot start the process instance.");

        this.setState(ProcessInstanceState.STARTING);

        ExecutionContext context = new ExecutionContextImpl(this);
        ExecutionThread thread = new ExecutionThread(this.processScope, context);
        thread.start();
        this.mainThread = thread;
        while (initialStartingActivity == null) {
            try {
                wait();
            } catch (InterruptedException exception) {
                // Ignore it.
            }
        }

        return initialStartingActivity;

    }

    /**
     * Send terminate signal to the process main acitivty. This is a blocking
     * method, that does not return until the process has terminated.
     */
    public void terminate() {
        this.run();
        processScope.terminate();

    }

    /**
     * Sending the exit signal to the process. This is the same as if the exit
     * activity was executed.
     */
    public void exit() {
        setState(ProcessInstanceState.EXITING);
        processScope.terminate();
    }

    /**
     * @return the initialStartingActivity
     */
    public Activity getInitialStartingActivity() {
        return initialStartingActivityConfiguration;
    }

    /**
     * @return the processContext
     */
    public ProcessContext getProcessContext() {
        return processContext;
    }

    /**
     * @return the processScope
     */
    public Scope getProcessScope() {
        return processScope;
    }

    /**
     * @return the state
     */
    public ProcessInstanceState getState() {
        return state;
    }

    private void setState(ProcessInstanceState newState) {
        ProcessInstanceState oldState = this.state;
        this.state = newState;
        for (ProcessInstanceListener listener : listeners) {
            listener.stateChange(this, oldState, newState);
        }
    }

    /**
     * Used only once
     * 
     * @param initialStartingActivity
     */
    public synchronized void setInitialStartingActivity(InboundMessageActivity initialStartingActivity) {
        if (this.initialStartingActivity != null)
            throw new IllegalStateException("Initial starting activity is already set.");

        this.initialStartingActivity = initialStartingActivity;

        this.notifyAll();

    }

    /**
     * Add the object to the list of object that are notified when the process
     * instance enters RUNNING state.
     * 
     * @param object
     */
    public void notifyOnRunningState(Object object) {
        notifyOnRunningStateList.add(object);
    }

    /**
     * Notification from the autonomous initial starting activity, that is has
     * completed. THis will make the process instance state change from STARTING
     * to RUNNING and notify the other starting activities waiting.
     */
    public void setInitialStartingActivityComplete() {
        this.setState(ProcessInstanceState.RUNNING);

        for (Object object : notifyOnRunningStateList) {
            object.notifyAll();
        }

    }

    public void stateChanged(com.beepell.activity.Activity activity, ActivityState oldState, ActivityState newState) {
        logger.log(Level.INFO, activity.getClass().getSimpleName() + " activity is '" + newState + "'.");
        for (StateChangeListener listener : activityListeners) {
            listener.stateChanged(activity, oldState, newState);
        }

    }

    /**
     * @return the initialOnMessage
     */
    public OnMessage getInitialOnMessage() {
        return initialOnMessage;
    }

    public void addedChild(com.beepell.activity.Activity parent, com.beepell.activity.Activity child) {
        for (StateChangeListener listener : activityListeners) {
            listener.addedChild(parent, child);
        }
    }

    /**
     * Add Activity State Change Listener
     * 
     * @param listner
     */
    public void addActivityStateChangeListener(StateChangeListener listner) {
        this.activityListeners.add(listner);
    }

    /**
     * Remove Activity State Change Listener
     * 
     * @param listner
     */
    public void removeActivityStateChangeListener(StateChangeListener listner) {
        this.listeners.remove(listner);
    }

    /**
     * Add Process Instance Listener
     * 
     * @param listener
     */
    public final void addListener(ProcessInstanceListener listener) {
        listeners.add(listener);
    }

    /**
     * Remove Process Instance Listener
     * 
     * @param listener
     */
    public final void removeListner(ProcessInstanceListener listener) {
        listeners.remove(listener);
    }

    /**
     * Makes the only one process thread take a step in the execution.
     */
    public synchronized void step() {
        if (run)
            return;

        this.step = 1;
        this.notifyAll();
    }

    /**
     * Makes all process threads take one step forward.
     */
    public synchronized void stepAll() {
        if (run)
            return;

        this.step = waiting.size();
        this.notifyAll();
    }

    /**
     * Get list of all activities waiting for user to step forward.
     * 
     * @return list of all waiting activities.
     */
    public synchronized List<com.beepell.activity.Activity> getWaitingActivities() {
        return waiting;
    }

    private boolean inFirst(int number, Object object) {
        for (int i = 0; i < number; i++)
            if (waiting.get(i) == object)
                return true;
        return false;
    }

    /**
     * called by activities to allow user controlled execution (STEP MODE)
     * 
     * @param activity the activity waiting to execute
     */
    public synchronized void next(com.beepell.activity.Activity activity) {
        if (run) {
            return;
        }

        this.waiting.add(activity);
        for (ProcessInstanceListener listener : listeners) {
            listener.stepExpected(this);
        }

        while (!inFirst(step, activity) && !run) {
            try {
                wait();
            } catch (Exception exception) {
            }
        }
        this.waiting.remove(activity);
        step--;

        if (waiting.size() > 0) {
            for (ProcessInstanceListener listener : listeners) {
                listener.stepExpected(this);
            }
        }
    }

    /**
     * Set process instance in RUN mode.
     */
    public synchronized void run() {
        this.run = true;
        this.notifyAll();
    }

    /**
     * Set process instance in STEP mode.
     */
    public synchronized void pause() {
        this.run = false;
        this.step = 0;
    }

    /**
     * call back for execute thread to complete the process instance.
     */
    public void complete() {
        switch (this.processScope.getState()) {
        case COMPLETED:
            setState(ProcessInstanceState.COMPLETED);
            break;
        case TERMINATED:
            setState(ProcessInstanceState.STOPPED);
            break;
        case FAILED:
            setState(ProcessInstanceState.FAILED);
            break;

        default:
            return;
        }

    }

    /**
     * Gets the main execution thread
     * 
     * @return the main execution thread
     */
    public Thread getMainThread() {
        return mainThread;
    }

    /**
     * Returns true if the instance is executing in running mode.
     * 
     * @return true if the instance is executing in running mode.
     */
    public boolean isRunningMode() {
        return this.run;
    }

    /**
     * Add handler to instance logger.
     * 
     * @param logHandler
     */
    public void addLogHandler(Handler logHandler) {
        logger.addHandler(logHandler);
    }

    /**
     * Remove handler from instance logger.
     * 
     * @param logHandler
     */
    public void removeLogHandler(Handler logHandler) {
        logger.removeHandler(logHandler);
    }

    /**
     * Log message.
     * @param level
     * @param message
     */
    public void log(Level level, String message) {
        logger.log(level, message);
    }
}
