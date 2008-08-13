package com.beepell.activity.structured;

import com.beepell.activity.ActivityState;
import com.beepell.exceptions.BPELFault;
import com.beepell.expression.BooleanExpression;
import com.beepell.model.Activity;
import com.beepell.model.WhileActivity;

/**
 * The while activity provides for repeated execution of a contained activity.
 * The contained activity is executed as long as the Boolean condition evaluates
 * to true at the beginning of each iteration.
 * 
 * @author Tim Hallwyl
 */
public class While extends AbstractStructuredActivity {

    private final BooleanExpression condition;
    private final Activity activity;
    
    /**
     * Create a While Activity.
     * @param configuration
     */
    public While(WhileActivity configuration) {
        super(configuration);
        this.condition = configuration.getConditionExpression();
        this.activity = configuration.getActivity();
        
        this.setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() throws BPELFault {
        
        while (condition.evaluate(context)) {
            
            if (getState() == ActivityState.TERMINATING)
                return;
            else
                execute(activity);
            
        }
    }
}
