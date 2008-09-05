package com.beepell.broker;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


/**
 * A message queue class.
 * 
 * @author Tim Hallwyl
 * 
 */
public class MessageQueue {

    protected final List<Message> queue = new ArrayList<Message>();

    private final List<QueueListener> listeners = new ArrayList<QueueListener>();

    private void fireAddedMessageEvent(Message message) {
        for (QueueListener listener : this.listeners) {
            listener.messageAdded(message);
        }
    }
    
    private void fireRemovedMessageEvent(Message message) {
        for (QueueListener listener : this.listeners) {
            listener.messageRemoved(message);
        }
    }
    
    /**
     * Adds a listener.
     * 
     * @param listener
     */
    public void addListener(QueueListener listener) {
        this.listeners.add(listener);
    }
    
    /**
     * Removes a listener. 
     * 
     * @param listener
     */
    public void removeListener(QueueListener listener) {
        this.listeners.remove(listener);
    }

    /**
     * Adds a message to the queue.
     * 
     * @param message the message to be added.
     */
    public synchronized void add(Message message) {
        this.queue.add(message);
        this.fireAddedMessageEvent(message);
    }

    /**
     * Gets an iterator over all messages in the queue belonging to the given
     * operation.
     * 
     * @param operation The operation name to iterate over.
     * @return An Iterator of Messages.
     */
    public synchronized Iterator<Message> getIterator(final String operation) {
        return new Iterator<Message>() {

            int index = -1;
            List<Message> queue = MessageQueue.this.queue;

            @Override
            public boolean hasNext() {
                for (int i = this.index + 1; i < this.queue.size(); i++)
                    if (this.queue.get(i).getOperationName().equals(operation))
                        return true;

                return false;
            }

            @Override
            public Message next() {
                for (int i = this.index + 1; i < this.queue.size(); i++)
                    if (this.queue.get(i).getOperationName().equals(operation)) {
                        this.index = i;
                        return this.queue.get(i);
                    }

                return null;
            }

            @Override
            public void remove() {
                MessageQueue.this.remove(this.index);
                this.index--;
            }

        };
    }

    /**
     * Gets an Iterator over all messages in the queue.
     * 
     * @return An Iterator of Messages.
     */
    public synchronized Iterator<Message> getIterator() {
        return this.queue.iterator();
    }

    /**
     * Remove the Message from the queue.
     * 
     * @param index Index of the Message to be removed.
     * @return the Message removed.
     */
    public synchronized Message remove(int index) {
        Message message = this.queue.remove(index);
        fireRemovedMessageEvent(message);
        return message;
    }

    /**
     * Remove the Message from the queue.
     * 
     * @param message The Message to be removed.
     * @return True if the message was removed, false if it was not found in the
     *         queue.
     */
    public synchronized boolean remove(Message message) {
        fireRemovedMessageEvent(message);
        return this.queue.remove(message);
    }
}
