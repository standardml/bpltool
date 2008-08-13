package com.beepell.execution;
import com.beepell.activity.Activity;
import com.beepell.activity.structured.ForEach;


/**
 * Thread class to execute concurrent activities in Flow and ForEach.
 * @author Tim Hallwyl
 *
 */
public class ExecutionThread extends Thread {

    private final Activity activity;
    private final ExecutionContext context;
    private final ForEach notifyWhenFinished;

    /**
     * Create an execution thread.
     * @param activity
     * @param context
     * @param notifyWhenFinished 
     */
    public ExecutionThread(Activity activity, ExecutionContext context, ForEach notifyWhenFinished) {
        this.activity = activity;
        this.context = context;
        this.notifyWhenFinished = notifyWhenFinished;
    }
    
    /**
     * Create an execution thread.
     * @param activity
     * @param context
     */
    public ExecutionThread(Activity activity, ExecutionContext context) {
        this.activity = activity;
        this.context = context;
        this.notifyWhenFinished = null;
    }
    
    public synchronized void run() {
        
        try {
            activity.execute(context);            
            
            if (notifyWhenFinished != null)
                notifyWhenFinished.complete();
            
            if (activity.equals(context.getInstance().getProcessScope()))
                context.getInstance().complete();
            
        } catch (Exception exception) {
            System.err.println("SERVERE: Failed to execute new thread.");
            exception.printStackTrace();
        }
        
    }
    
    /**
     * @return the activity
     */
    public Activity getActivity() {
        return activity;
    }


    
    /**
     * @return the context
     */
    public ExecutionContext getContext() {
        return context;
    }
    
    
}
