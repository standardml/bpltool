package com.beepell.execution.transaction;

import java.util.LinkedList;
import java.util.Queue;

/**
 * @author Tim Hallwyl
 */
public class TransactionImpl extends Thread implements Transaction {
    
    private String name;
    
    private class Command {
        /**
         * @param object
         * @param type
         */
        public Command(Object object, LockType type) {
            this.object = object; this.type = type;
        }
        /** object to lock */
        public Object object;

        /** lock type to acquire */
        public LockType type;
    }
    
    private Queue<Command> commands = new LinkedList<Command>(); 
    

    private String state = "idle";

    private final LockManager lockManager = LockManager.getInstance();

    private final Transaction parent;

    TransactionImpl(Transaction parent, String name) {
        this.parent = parent;
        this.name = name;
    }

    public synchronized void abort() {
        lockManager.release(this);
    }

    public synchronized void commit() {
        if (parent != null)
            lockManager.inherit(this);
        else
            lockManager.release(this);
    }

    public Transaction getParent() {
        return parent;
    }

    public synchronized void run() {
        Command command;
        System.out.println("Transaction ready");
        try {
            
            
            while (true) {
                if (commands.isEmpty())
                    wait();
                command = commands.remove();
                System.out.println(this.getName() + " is " + command.type + " locking " + command.object);
                System.out.flush();
                state = "waiting";
                lockManager.lock(command.object, this, command.type);
                state = "idle";
            }
            
            
        } catch (InterruptedException exception) {
            exception.printStackTrace();
            /* do nothing */
        } finally {
            System.out.println("Transaction stopped");
        }

    }

    /**
     * @param object
     */
    public synchronized void writeLock(Object object) {
        this.commands.add(new Command(object, LockType.WRITE));        
        this.notify();
    }

    /**
     * @param object
     */
    public synchronized void readLock(Object object) {
        this.commands.add(new Command(object, LockType.READ));        
        this.notify();
    }

    /**
     * @return the state
     */
    public String getTransactionState() {
        return state;
    }

    
    /**
     * @return the name
     */
    public String getTransactionName() {
        return name;
    }

    public void writeLockPartnerLink(String partnerLink) {
        // TODO Auto-generated method stub
        
    }

    public void writeLockVariable(String variableName) {
        // TODO Auto-generated method stub
        
    }
}
