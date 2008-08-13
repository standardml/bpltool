package com.beepell.activity.structured;

import com.beepell.activity.Activity;
import com.beepell.activity.ActivityState;
import com.beepell.exceptions.BPELFault;
import com.beepell.exceptions.CompletionConditionFailure;
import com.beepell.exceptions.InvalidBranchCondition;
import com.beepell.execution.ExecutionThread;
import com.beepell.expression.UnsignedIntegerExpression;
import com.beepell.model.ForEachActivity;
import com.beepell.model.ScopeActivity;

/**
 * The forEach activity will execute its contained scope activity exactly N+1
 * times where N equals the finalCounterValue minus the startCounterValue.
 * 
 * @author Tim Hallwyl
 */
public class ForEach extends AbstractStructuredActivity {

    private final UnsignedIntegerExpression startCounterExpression;

    private final UnsignedIntegerExpression finalCounterExpression;

    private final ScopeActivity scope;

    private final boolean parallel;

    private final String counterName;

    private final boolean completionCondition;

    private final UnsignedIntegerExpression completionConditionExpression;

    private final boolean successfulBranchesOnly;

    /**
     * @param configuration
     */
    public ForEach(ForEachActivity configuration) {
        super(configuration);
        this.startCounterExpression = configuration.getStartCounterExpression();
        this.finalCounterExpression = configuration.getFinalCounterExpression();
        this.scope = configuration.getScope();
        this.parallel = configuration.isParallel();
        this.counterName = configuration.getCounterName();

        if (configuration.getCompletionCondition() != null) {
            completionCondition = true;
            completionConditionExpression = configuration.getCompletionCondition().getConditionExpression();
            if (configuration.getCompletionCondition().getBranchesElementNode().getAttribute("successfulBranchesOnly").equals("yes"))
                successfulBranchesOnly = true;
            else
                successfulBranchesOnly = false;

        } else {
            completionCondition = false;
            completionConditionExpression = null;
            successfulBranchesOnly = false;
        }

        this.setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() throws BPELFault {

        final long startCounterValue = startCounterExpression.evaluate(context);
        final long finalCounterValue = finalCounterExpression.evaluate(context);
        final long n = finalCounterValue - startCounterValue;

        long b = 0;
        if (completionCondition) {
            b = completionConditionExpression.evaluate(context);

            // if its value is larger than the number of directly enclosed
            // activities N+1, then the standard bpel:invalidBranchCondition
            // fault MUST be thrown.

            if (b > n + 1)
                throw new InvalidBranchCondition("Completion condition evaluated to " + b + ", but there is only " + (n + 1) + " directly enclosed activities.");
        }

        log.info("ForEach start at " + startCounterValue + " end at " + finalCounterValue + " complete with " + b);
        
        // If the startCounterValue is greater than the finalCounterValue,
        // then the child scope activity MUST NOT be performed and the
        // forEach activity is complete.
        if (startCounterValue > finalCounterValue)
            return;

        if (parallel) {
            Count count;
            ExecutionThread[] threads = new ExecutionThread[new Long(n + 1).intValue()];

            for (int i = 0; i < n + 1; i++) {
                if (getState() == ActivityState.TERMINATING)
                    return;
                else
                    threads[i] = executeThread(scope, context, this, counterName, startCounterValue + i, context.getSchemaRepository());
            }

            while (true) {
                try { wait(); } catch (InterruptedException exception) { /* do nothing */  }

                if (getState() == ActivityState.TERMINATING)
                    return;

                if (allDone(threads))
                    return;

                // At the end of execution of each directly enclosed scope
                // activity, the number of completed children is compared to B,
                // the value of the branches expression.
                if (completionCondition) {
                    // Count alive and completed threads (Scopes)
                    count = count(threads);

                    log.info("ForEach completed " + count.complete + ", "+ count.alive +" is still running");
                    
                    // If at least B children have completed, the
                    // completionCondition is triggered: No further children
                    // will be started, and currently running children will be
                    // terminated
                    if (count.complete >= b) {
                        log.info("ForEach completion condition reached, termination running childs.");
                        for (int i = 0; i < threads.length; i++) {
                            threads[i].getActivity().terminate();
                        }
                        return;
                    }

                    // If upon completion of a directly enclosed scope
                    // activity, it can be determined that the
                    // completionCondition can never be true, the standard
                    // bpel:completionConditionFailure fault MUST be thrown.
                    if (successfulBranchesOnly) {
                        if ((b - count.complete) > count.alive)
                            throw new CompletionConditionFailure("Completion requires " + b + " successful branches. " + count.complete + " branches have completed successfully and only " + count.alive + " are still alive.");
                    }
                }
            }

        } else {

            // The activity is a serial forEach. The enclosed scope activity
            // MUST be executed N+1 times, each instance starting only after the
            // previous repetition is complete.
            int completed = 0;
            for (int i = 0; i < n + 1; i++) {
                if (getState() == ActivityState.TERMINATING)
                    return;

                Scope activity = execute(scope, context, counterName, startCounterValue + i, context.getSchemaRepository());
                if (!successfulBranchesOnly || activity.getState() == ActivityState.COMPLETED) {
                    completed++;
                }

                // If at least B children have completed, the
                // completion condition is triggered: No further children will
                // be started.
                if (completed >= b)
                    return;

            }

        }

    }

    private boolean allDone(Thread[] threads) {

        for (Thread thread : threads) {
            if (thread.isAlive())
                return false;
        }
        return true;
    }
    
    private class Count {
        int alive = 0; // Scopes still running
        int complete = 0; // Scopes complete.
    }
    
    private Count count(ExecutionThread threads[]) {
        Count count = new Count();
        for (int i = 0; i < threads.length; i++) {
            if (threads[i].getActivity().getState() != ActivityState.RUNNING) {                            
                Activity activity = threads[i].getActivity();
                if (!successfulBranchesOnly || activity.getState() == ActivityState.COMPLETED)
                    count.complete++;
            } else {
                count.alive++;
            }
        }
        return count;
    }
    
    /**
     * Called by ExecutionThread to notify that a thread has completed.
     *
     */
    public synchronized void complete() {
        notifyAll();
    }
    
}
