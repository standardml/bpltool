package com.beepell.execution;

/**
 * 
 * @author Tim Hallwyl
 *
 */
public interface ProcessInstanceListener {

    /**
     * Process Instance state has changed
     * @param instance
     * @param oldState
     * @param newState
     */
    public void stateChange(ProcessInstance instance, ProcessInstanceState oldState, ProcessInstanceState newState);
    
    /**
     * User input to step execution is expected.
     * @param instance
     */
    public void stepExpected(ProcessInstance instance);
    
}
