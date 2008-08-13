package com.beepell.execution.transaction;

import junit.framework.TestCase;

/**
 * Test nested transactions.
 * 
 * @author Tim Hallwyl
 */
public class NestedTransactionTest extends TestCase {

    /**
     * Test flat transaction un-conflicting operations.
     */
    public void testNested() {
        try {
            Object o1 = new Object();

            TransactionImpl top = new TransactionImpl(null, "top");
            TransactionImpl t1 = new TransactionImpl(top, "t1");
            TransactionImpl t2 = new TransactionImpl(top, "t2");
            TransactionImpl other = new TransactionImpl(null, "other");

            top.start();
            t1.start();
            t2.start();
            other.start();

            Thread.sleep(500);
            assertEquals("idle", top.getTransactionState());
            assertEquals("idle", t1.getTransactionState());
            assertEquals("idle", t2.getTransactionState());
            assertEquals("idle", other.getTransactionState());

            // nobody has any locks

            t1.readLock(o1);
            t2.readLock(o1);
            Thread.sleep(500);
            assertEquals("idle", t1.getTransactionState());
            assertEquals("idle", t2.getTransactionState());

            // now t1 and t2 share a read lock on o1

            t2.writeLock(o1);
            Thread.sleep(500);
            assertEquals("idle", t1.getTransactionState());
            assertEquals("waiting", t2.getTransactionState());

            // t1 and t2 still share the read lock on o1, but t2 is waiting to
            // promote is

            t1.commit();
            Thread.sleep(500);
            assertEquals("idle", t2.getTransactionState());
            assertEquals("idle", other.getTransactionState());

            // top inherits t1's read lock on o1, t2 has promoted it

            other.writeLock(o1);
            Thread.sleep(500);
            assertEquals("idle", t2.getTransactionState());
            assertEquals("waiting", other.getTransactionState());

            // t2 has a write lock on o1, that other is waiting for

            t2.abort();
            Thread.sleep(500);
            assertEquals("idle", top.getTransactionState());
            assertEquals("waiting", other.getTransactionState());

            // t2 aborted but top still has the lock on o1

            top.commit();
            Thread.sleep(500);
            assertEquals("idle", other.getTransactionState());

            // other now has a write lock on o1

        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }

    }

    /**
     * Bugfix test: missing if clause in inherit. 
     *
     */
    public void testBug01() {
        try {
            Object o1 = new Object();
            Object o2 = new Object();

            TransactionImpl top1 = new TransactionImpl(null, "top1");
            TransactionImpl t1 = new TransactionImpl(top1, "t1");

            TransactionImpl top2 = new TransactionImpl(null, "top2");
            TransactionImpl t21 = new TransactionImpl(top2, "t2");
            TransactionImpl t22 = new TransactionImpl(top2, "t2");

            top1.start();
            t1.start();
            top2.start();
            t21.start();
            t22.start();

            top1.readLock(o1);
            Thread.sleep(500);
            t1.readLock(o1);

            top2.readLock(o2);
            Thread.sleep(500);
            t21.readLock(o2);
            Thread.sleep(500);
            t22.writeLock(o2);
           
            Thread.sleep(500);
            assertEquals("idle", t21.getTransactionState());
            assertEquals("waiting", t22.getTransactionState());
            
            Thread.sleep(500);
            t1.commit();
            t21.abort();
            Thread.sleep(500);
            //assertEquals("idle", t21.getTransactionState());
            assertEquals("idle", t22.getTransactionState());
            

        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }
    }
    
 
}
