package com.beepell.execution.transaction;

/**
 * 
 * @author Tim Hallwyl
 *
 */
public interface Transaction {

    /**
     * Gets the parent transaction if any, otherwise null is returned.
     * @return the parent transaction if any, otherwise null
     */
    public Transaction getParent();
    
    /**
     * Commits the transaction. 
     *
     */
    public void commit();
    
    /**
     * Aborts the transaction.
     *
     */
    public void abort();
    
    /**
     * Explicit write lock a variable.
     * @param variableName
     */
    public void writeLockVariable(String variableName);
    
    /**
     * Explicit write lock a partner link.
     * @param partnerLink
     */
    public void writeLockPartnerLink(String partnerLink);
}
