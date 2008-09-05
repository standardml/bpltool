package com.beepell.broker;

/**
 * @author Tim Hallwyl
 * 
 */
public interface QueueListener {

    /**
     * The method is called when a message has been added to the queue.
     * 
     * @param message The added message.
     */
    public void messageAdded(Message message);

    /**
     * This method is called when a message has been removed.
     * 
     * @param message The removed message.
     */
    public void messageRemoved(Message message);

}
