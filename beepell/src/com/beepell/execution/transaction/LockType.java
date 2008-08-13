package com.beepell.execution.transaction;

/**
 * There are two types of locks: (shared) read lock and (exclusive) write lock.
 * @author Tim Hallwyl
 *
 */
public enum LockType {
    /** (shared) read lock */
    READ,
    
    /** (exclusive) write lock */
    WRITE
}
