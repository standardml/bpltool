package com.beepell.activity.structured;

import com.beepell.activity.ActivityState;
import com.beepell.exceptions.BPELFault;
import com.beepell.expression.BooleanExpression;
import com.beepell.model.Activity;
import com.beepell.model.RepeatUntilActivity;

/**
 * The repeatUntil activity provides for repeated execution of a contained
 * activity. The contained activity is executed until the given Boolean
 * condition becomes true. The condition is tested after each execution of the
 * body of the loop. In contrast to the while activity, the repeatUntil loop
 * executes the contained activity at least once.
 * 
 * @author Tim Hallwyl
 */
public class RepeatUntil extends AbstractStructuredActivity {
   
    private final BooleanExpression condition;
    private final Activity activity;
    
    /**
     * Create a RepeatUntil Activity.
     * @param configuration
     */
    public RepeatUntil(RepeatUntilActivity configuration) {
        super(configuration);
        this.condition = configuration.getConditionExpression();
        this.activity = configuration.getActivity();

        this.setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() throws BPELFault {

        do {
            
            if (getState() == ActivityState.TERMINATING)
                return;
            else
                execute(activity);
            
        } while (!condition.evaluate(context));
        
    }

}
