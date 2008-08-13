package com.beepell.activity.basic;

import com.beepell.activity.ActivityState;
import com.beepell.model.ExitActivity;

/**
 * <p>
 * The exit activity is used to immediately end the business process instance.
 * All currently running activities MUST be ended immediately without involving
 * any termination handling, fault handling, or compensation behavior.
 * 
 * @author Tim Hallwyl
 */
public class Exit extends AbstractBasicActivity {

    /**
     * Configure the activity.
     * 
     * @param configuration
     */
    public Exit(ExitActivity configuration) {
        super(configuration);
        setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() {
        
        context.exit();

    }

}
