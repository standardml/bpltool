package com.beepell.activity.basic;

import java.util.Calendar;
import java.util.GregorianCalendar;

import javax.xml.datatype.Duration;
import javax.xml.datatype.XMLGregorianCalendar;

import com.beepell.activity.ActivityState;
import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.SubLanguageExecutionFault;
import com.beepell.expression.DeadlineExpression;
import com.beepell.expression.DurationExpression;
import com.beepell.model.WaitActivity;

/**
 * <p>
 * The wait activity specifies a delay for a certain period of time or until a
 * certain deadline is reached (see section 8.3. Expressions for the grammar of
 * duration expressions and deadline expressions). If the specified duration
 * value in for is zero or negative, or a specified deadline in until has
 * already been reached or passed, then the wait activity completes immediately.
 * [10.7]
 * 
 * @author Tim Hallwyl
 */
public class Wait extends AbstractBasicActivity {

    private DurationExpression forExpression;

    private DeadlineExpression untilExpression;
    
    private final static Calendar calendar = GregorianCalendar.getInstance();

    /**
     * Configure the activity.
     * 
     * @param configuration
     */
    public Wait(WaitActivity configuration) {
        super(configuration);
        forExpression = configuration.getForExpression();
        untilExpression = configuration.getUntilExpression();
        setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() throws InvalidExpressionValue, SubLanguageExecutionFault {
        try {

            long time = 0; // number of ms to wait
            long wake = 0; // when to wake: ms since epoch

            if (forExpression != null) {

                Duration duration = forExpression.evaluate(context);
                time = duration.getTimeInMillis(calendar);
                wake = System.currentTimeMillis() + time;
            }

            if (untilExpression != null) {
                XMLGregorianCalendar calendar = untilExpression.evaluate(context);
                wake = calendar.toGregorianCalendar().getTimeInMillis();
                time = wake - System.currentTimeMillis();
            }

            while (time > 0) {
                log.info("'" + Thread.currentThread().getName() + "' is waiting for " + time +"ms.");
                wait(time);
                time = wake - System.currentTimeMillis();

                if (getState() == ActivityState.TERMINATING)
                    return;
            }

        } catch (InterruptedException e) {
            return;
        }
    }

}
