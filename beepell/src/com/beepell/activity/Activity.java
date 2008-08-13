package com.beepell.activity;

import com.beepell.execution.ExecutionContext;

/**
 * Common interface for all activities.
 * 
 * @author Tim Hallwyl
 */
public interface Activity {

    /**
     * Gets the current activity state.
     * 
     * @see ActivityState
     * @return the current activity state.
     */
    public abstract ActivityState getState();

    /**
     * Add a state change listener. It will receive notifications for this and
     * all child activities.
     * 
     * @param listener
     */
    public abstract void addListener(StateChangeListener listener);

    /**
     * Removes the listener from the notification list.
     * 
     * @param listener
     */
    public abstract void removeListner(StateChangeListener listener);

    /**
     * Executes the activity. Used by an engine.
     * 
     * @param context Object providing the execution context.
     * @throws Exception
     */
    public abstract void execute(ExecutionContext context);

    /**
     * Makes the activity terminate.
     */
    public abstract void terminate();

}