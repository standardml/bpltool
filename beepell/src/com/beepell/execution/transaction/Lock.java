package com.beepell.execution.transaction;

import java.util.Vector;

/**
 * The lock for an object.
 * 
 * @author Tim Hallwyl
 */
public class Lock {

    private final Object object;

    /*
     * TODO: Vector is synchronized and an ArrayList is not. Since the Vector is
     * synchronized it has overhead for each call to the Vector. But we access
     * it only from within synchonized methods, sp maybe we might as well use
     * ArrayList?
     */
    private final Vector<Transaction> holders;

    private LockType lockType;

    /**
     * Create a lock for an object.
     * 
     * @param object the object to be locked.
     */
    public Lock(Object object) {
        this.object = object;
        this.holders = new Vector<Transaction>();
        this.lockType = LockType.READ;

    }

    private boolean retainable(Transaction transaction) {
        if (holders.size() == 1 && holders.get(0).equals(transaction.getParent()))
            return true;

        if (holders.size() == 2 && holders.contains(transaction.getParent()) && holders.contains(transaction))
            return true;

        return false;
    }

    private boolean inConflict(Transaction transaction, LockType requested) {

        /* Nobody holds this lock */
        if (this.holders.isEmpty())
            return false;

        /* This is a shared read lock */
        if (this.lockType == LockType.READ && requested == LockType.READ)
            return false;

        /* This is a read lock that may be promoted */
        if (requested == LockType.WRITE && holders.size() == 1 && holders.get(0).equals(transaction))
            return false;

        /*
         * This is a write lock owned by the parent transaction, request is a
         * read lock
         */
        if (lockType == LockType.WRITE && retainable(transaction) && requested == LockType.READ) {
            return false;
        }

        /*
         * This is a read or write lock owned by the parent transaction, request
         * is a write lock
         */
        if (retainable(transaction) && requested == LockType.WRITE) {
            return false;
        }

        return true;
    }

    /**
     * Acquire a lock.
     * 
     * @param transaction the transaction requesting a lock
     * @param requested the lock type requested
     */
    public synchronized void aquire(Transaction transaction, LockType requested) {

        /* If the transaction already has the lock, just return */
        if (holders.contains(transaction) && lockType == requested)
            return;

        while (inConflict(transaction, requested)) {
            /* another transaction holds the lock in a conflicting mode */
            System.out.println("INFO: Waiting for " + this.lockType + " on " + this.object.getClass().getSimpleName() + " to be released.");
            try {
                wait();
            } catch (InterruptedException exception) {
                /* do nothing */
            }
        }

        /* no longer in conflicting mode */
        if (!this.holders.contains(transaction))
            this.holders.add(transaction);

        /* never demote a lock during an acquire */
        if (requested == LockType.WRITE)
            this.lockType = requested;

    }

    /**
     * Release the transactions lock.
     * 
     * @param transaction
     */
    public synchronized void release(Transaction transaction) {
        if (this.holders.remove(transaction)) {
            
            /* demote lock to READ if nobody uses it */
            if (this.holders.isEmpty())
                lockType = LockType.READ;

            notifyAll();
        }
    }

    /**
     * Inherit lock from child to parent. This is used when a successful
     * (committed) nested transaction completes. Instead of releasing its locks,
     * they are inherited to the parent transaction.
     * 
     * @param transaction the committed (nested child) transaction
     */
    public synchronized void inherit(Transaction transaction) {
        if (this.holders.remove(transaction)) {
            this.holders.add(transaction.getParent());
            notifyAll();
        }
    }

    /**
     * @return the object
     */
    public Object getObject() {
        return object;
    }

    /**
     * Gets the current Lock type.
     * @return the current Lock type.
     */
    public LockType getType() {
        return lockType;
    }
    
}
