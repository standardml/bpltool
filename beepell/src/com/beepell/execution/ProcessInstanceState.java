package com.beepell.execution;

/**
 * The life cycle states of an process instance.
 * 
 * @author Tim Hallwyl
 */
public enum ProcessInstanceState {
    
    /**
     * State until execution starts. 
     */
    INITIALIZING,
    
    /**
     * State until the initial starting activity has completed. Only scope,
     * sequence, flow and the initial starting activity may run in this state.
     */
    STARTING,

    /**
     * Normal running state, entered after the initial starting activity has
     * completed. All activites may execute in this state.
     */
    RUNNING,

    /**
     * Successful completion of the process instance.
     */
    COMPLETED,

    /**
     * The process instance failed.
     */
    FAILED,

    /**
     * Process Instance is exiting
     */
    EXITING, 
    
    /**
     * The instance was stopped by an exit activity or by external force.
     */
    STOPPED

}
