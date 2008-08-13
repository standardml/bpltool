package com.beepell.activity;

/**
 * Interface for activity state change listeners.
 * @author Tim Hallwyl
 *
 */
public interface StateChangeListener {

    /**
     * Call-back method invoked on a state change in an activity.
     * @param activity the activity that changed state-
     * @param oldState the state before the change. 
     * @param newState the state after the change.
     */
    public void stateChanged(Activity activity, ActivityState oldState, ActivityState newState);
 
    /**
     * When a structured activity creates a new child activity for execution.  
     * @param parent
     * @param child
     */
    public void addedChild(Activity parent, Activity child);
}
