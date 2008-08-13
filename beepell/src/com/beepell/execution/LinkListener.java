package com.beepell.execution;

/**
 * Interface for object who listen for link changes.
 * @author Tim Hallwyl
 *
 */
public interface LinkListener {

    /**
     * Messaged on link changed.
     * @param linkName
     */
    public void linkChanged(String linkName);
    
}
