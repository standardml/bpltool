package com.beepell.activity.structured;

import java.util.List;

import com.beepell.activity.ActivityState;
import com.beepell.model.Activity;
import com.beepell.model.SequenceActivity;

/**
 * A sequence activity contains one or more activities that are performed
 * sequentially, in the lexical order in which they appear within the sequence
 * element. The sequence activity completes when the last activity in the
 * sequence has completed.
 * 
 * @author Tim Hallwyl
 */
public class Sequence extends AbstractStructuredActivity {

    private final List<Activity> activities;

    /**
     * Create a Sequence Activity.
     * @param configuration
     */
    public Sequence(SequenceActivity configuration) {
        super(configuration);
        this.activities = configuration.getActivity();
        this.setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() {

        for (Activity activity : activities) {
            if (this.getState() == ActivityState.TERMINATING)
                return;

            execute(activity);
        }
    }
}
