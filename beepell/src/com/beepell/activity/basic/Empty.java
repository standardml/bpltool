package com.beepell.activity.basic;

import com.beepell.activity.ActivityState;
import com.beepell.model.EmptyActivity;

/**
 * There is often a need to use an activity that does nothing, for example when
 * a fault needs to be caught and suppressed. The empty activity is used for
 * this purpose. Another use of empty is to provide a synchronization point in a
 * flow. [10.8]
 * 
 * @author Tim Hallwyl
 */
public class Empty extends AbstractBasicActivity {

    /**
     * Configure the activity.
     * @param configuration
     */
    public Empty(EmptyActivity configuration) {
        super(configuration);
        setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() {
        // Do nothing.
    }

}
