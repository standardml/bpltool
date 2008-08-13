package com.beepell.execution.event;

import java.util.ArrayList;
import java.util.List;
import java.util.TimerTask;

import com.beepell.activity.structured.Scope;
import com.beepell.model.ScopeActivity;

/**
 * @author Tim Hallwyl
 */
public class AlarmEventHandler extends TimerTask {

    /**
     * the chain of enclosing scope or process elements of the event handler
     */
    private final Scope ancestor;

    /**
     * The scope directly defined within onAlarm
     */
    private final ScopeActivity associated;

    private List<Thread> threads = new ArrayList<Thread>();

    /**
     * Creates an onAlarm event handler.
     * 
     * @param associated
     * @param ancestor
     */
    public AlarmEventHandler(ScopeActivity associated, Scope ancestor) {
        this.associated = associated;
        this.ancestor = ancestor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.TimerTask#run()
     */
    @Override
    public void run() {
        threads.add(ancestor.executeAlarmEvent(associated));
    }

    /**
     * Wait for all running event instances to finish.
     */
    public synchronized void join() {
        boolean interrupted;
        for (Thread thread : threads) {
            do {
                try {
                    interrupted = false;
                    thread.join();
                } catch (InterruptedException exception) {
                    interrupted = true;
                }
            } while (interrupted);
        }
    }

}
