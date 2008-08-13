package com.beepell.execution.transaction;

import java.util.Enumeration;
import java.util.Hashtable;

/**
 * The Lock Manager Class - manages all locks.
 * @author Tim Hallwyl
 *
 */
public class LockManager {

    private static LockManager instance;
    
    private static final Hashtable<Object, Lock> locks = new Hashtable<Object, Lock>();

    private LockManager() {
        /* do nothing */
    }
    
    /**
     * Get the singleton instance of the global lock manager.
     * @return the lock manager
     */
    public static LockManager getInstance() {
        if (instance == null)
            instance = new LockManager();
        return instance;
    }
    
    /**
     * Sets a lock on the object for the transaction of the type.
     * @param object the object to lock
     * @param transaction the transaction that holds the lock
     * @param lockType READ or WRITE
     */
    public void lock(Object object, Transaction transaction, LockType lockType) {
        Lock lock;
        synchronized (this) {
            lock = locks.get(object);
            if (lock == null) {
                lock = new Lock(object);
                locks.put(object, lock);
            }                
        }
        lock.aquire(transaction, lockType);
    }
    
    /**
     * Releases all locks hold by the transaction.
     * @param transaction
     */
    public synchronized void release(Transaction transaction) {
        Enumeration<Lock> locks = LockManager.locks.elements();
        while (locks.hasMoreElements())
            locks.nextElement().release(transaction);
    }
    
    /**
     * Let the parent transaction all locks hold by the transaction.
     * @param transaction
     */
    public synchronized void inherit(Transaction transaction) {
        Enumeration<Lock> locks = LockManager.locks.elements();
        while (locks.hasMoreElements())
            locks.nextElement().inherit(transaction);
    }
    
    /**
     * Gets the lock type for an object.
     * @param object the locked object
     * @return the current lock type for that object.
     */
    public LockType getLockType(Object object) {
        Lock lock = locks.get(object);
        if (lock == null)
            return null;
        return lock.getType();
        
    }
}
