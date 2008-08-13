package com.beepell.execution.transaction;

import junit.framework.TestCase;

/**
 * Testing the lock manager and the locks.
 * 
 * @author Tim Hallwyl
 */
public class FlatTransactionTest extends TestCase {

    private static LockManager lockManager = LockManager.getInstance();

    /**
     * Test flat transaction un-conflicting operations.
     */
    public void testUnConflicting() {
        try {

            Transaction transaction1 = new TransactionImpl(null, "t1");
            Transaction transaction2 = new TransactionImpl(null, "t2");

            Object o1 = new Object();
            Object o2 = new Object();

            // Normal READ locks
            lockManager.lock(o1, transaction1, LockType.READ);
            lockManager.lock(o2, transaction2, LockType.READ);

            // Shared READ
            lockManager.lock(o1, transaction2, LockType.READ);

            lockManager.release(transaction1);

            // Promote to WRITE
            lockManager.lock(o1, transaction2, LockType.WRITE);

            // Relock same
            lockManager.lock(o1, transaction2, LockType.WRITE);

            lockManager.release(transaction2);

        } catch (Exception e) {
            e.printStackTrace();
            fail();
        }

    }

    /**
     * Test flat transaction waiting for read
     */
    public void testWaitForRead() {
        try {
            Object object1 = new Object();
            TransactionImpl transaction1 = new TransactionImpl(null, "t1");
            TransactionImpl transaction2 = new TransactionImpl(null, "t2");
            TransactionImpl transaction3 = new TransactionImpl(null, "t3");

            transaction1.start();
            transaction2.start();
            transaction3.start();

            Thread.sleep(500);
            assertEquals("idle", transaction1.getTransactionState());
            assertEquals("idle", transaction2.getTransactionState());
            assertEquals("idle", transaction3.getTransactionState());

            transaction1.writeLock(object1);
            Thread.sleep(500);
            transaction2.readLock(object1);
            Thread.sleep(500);
            transaction3.readLock(object1);

            Thread.sleep(500);
            assertEquals("idle", transaction1.getTransactionState());
            assertEquals("waiting", transaction2.getTransactionState());
            assertEquals("waiting", transaction3.getTransactionState());

            transaction1.commit();
            Thread.sleep(500);
            assertEquals("idle", transaction2.getTransactionState());
            assertEquals("idle", transaction3.getTransactionState());

        } catch (Exception exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }

    /**
     * Test flat transaction waiting for write
     */
    public void testWaitForWrite() {
        try {
            Object object1 = new Object();
            TransactionImpl transaction1 = new TransactionImpl(null, "t1");
            TransactionImpl transaction2 = new TransactionImpl(null, "t2");
            TransactionImpl transaction3 = new TransactionImpl(null, "t3");

            transaction1.start();
            transaction2.start();
            transaction3.start();

            Thread.sleep(500);
            assertEquals("idle", transaction1.getTransactionState());
            assertEquals("idle", transaction2.getTransactionState());
            assertEquals("idle", transaction3.getTransactionState());

            transaction1.readLock(object1);
            transaction2.readLock(object1);
            transaction3.writeLock(object1);

            Thread.sleep(500);
            assertEquals("idle", transaction1.getTransactionState());
            assertEquals("idle", transaction2.getTransactionState());
            assertEquals("waiting", transaction3.getTransactionState());

            transaction1.abort();
            Thread.sleep(500);
            assertEquals("idle", transaction2.getTransactionState());
            assertEquals("waiting", transaction3.getTransactionState());

            transaction2.commit();
            Thread.sleep(500);
            assertEquals("idle", transaction3.getTransactionState());

        } catch (Exception exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }
    }

}
