package com.beepell.activity.structured;

import java.util.Date;
import java.util.List;

import javax.xml.datatype.Duration;
import javax.xml.datatype.XMLGregorianCalendar;

import com.beepell.activity.ActivityState;
import com.beepell.exceptions.BPELFault;
import com.beepell.execution.ExecutionContext;
import com.beepell.execution.ProcessInstanceState;
import com.beepell.expression.DeadlineExpression;
import com.beepell.expression.DurationExpression;
import com.beepell.model.Activity;
import com.beepell.model.OnAlarmPick;
import com.beepell.model.OnMessage;
import com.beepell.model.PickActivity;

/**
 * Pick Activity.
 * 
 * @author Tim Hallwyl
 */
public class Pick extends AbstractStructuredActivity {

    private final List<OnAlarmPick> alarms;

    private final List<OnMessage> onMessages;

    private OnMessageHandler picked = null;

    /**
     * Create a Pick activity.
     * 
     * @param configuration
     */
    public Pick(PickActivity configuration) {
        super(configuration);
        this.alarms = configuration.getOnAlarm();
        this.onMessages = configuration.getOnMessage();
        setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() throws BPELFault {

        /*
         * When the process is in STARTING state, other starting activities must
         * wait until the initial starting activity has completed.
         */
        boolean isInitialStartingActivity = context.isInitialStartingActivity(this.configuration);
        if (!isInitialStartingActivity && context.getInstanceState() == ProcessInstanceState.STARTING) {
            try {
                context.notifyOnRunningState(this);
                while (context.getInstanceState() == ProcessInstanceState.STARTING) {
                    wait();
                }
            } catch (InterruptedException exception) {
                return;
            }
        }

        if (isInitialStartingActivity) {
            OnMessage onMessage = context.getInitialOnMessage();
            context.setInitialStartingActivityReady(new OnMessageHandler(onMessage, this));
            log.info("Pick activity is waiting to be picked (" + picked + ").");

            while (picked == null) {
                try {
                    wait();
                } catch (InterruptedException exception) {
                    /* do nothing */
                }
            }
            log.info("Pick activity picked message for operation '" + picked.getOperation() + "'.");
            execute(picked.getActivity());
            context.setInitialStartingActivityComplete(this);
            return;
        }
        /*
         * Set alarms, if any. We set them up first because an alarm may be
         * pre-tricked, requiring it to execute immediately.
         */
        long time = Long.MAX_VALUE; // number of ms to wait
        long wake = Long.MAX_VALUE; // when to wake: ms since epoch
        long t = 0, w = 0;
        Activity alarmActivity = null;
        DurationExpression forExpression;
        DeadlineExpression untilExpression;

        /*
         * Of all the OnAlarm set on this Pick only one of them may trick the
         * activity: the earliest of them. So we evaluate them all to see how
         * long time from now the earliest alarm is, and use this as time out
         * for receiving a message.
         */
        for (OnAlarmPick alarm : alarms) {
            forExpression = alarm.getDurationExpression();
            untilExpression = alarm.getDeadLineExpression();

            if (forExpression != null) {
                Duration duration = forExpression.evaluate(context);
                t = duration.getTimeInMillis(new Date());
                w = System.currentTimeMillis() + t;
            }

            if (untilExpression != null) {
                XMLGregorianCalendar calendar = untilExpression.evaluate(context);
                w = calendar.toGregorianCalendar().getTimeInMillis();
                t = w - System.currentTimeMillis();
            }

            if (t < time) {
                time = t;
                wake = w;
                alarmActivity = alarm.getActivity();
            }
        }

        /*
         * We do not engage OnMessageHandlers if the alarm is pretricked.
         */
        OnMessageHandler[] handlers = new OnMessageHandler[onMessages.size()];
        if (alarmActivity == null || time > 0) {
            int index = 0;
            for (OnMessage onMessage : onMessages) {
                handlers[index++] = new OnMessageHandler(onMessage, this);
            }
        }

        while (picked == null && (alarms.isEmpty() || time > 0)) {
            try {
                log.info("Pick timeout is " + time + "ms.");

                wait(time);
                time = wake - System.currentTimeMillis();

                if (getState() == ActivityState.TERMINATING)
                    return;

            } catch (InterruptedException exception) {
                /* do nothing */
            }

        }

        /*
         * Cancel all (other) OnMessage handlers.
         */
        for (int index = 0; index < handlers.length; index++) {
            if (!handlers[index].equals(picked))
                handlers[index].cancel();
        }

        if (picked == null)
            execute(alarmActivity);
        else
            execute(picked.getActivity());

    }

    /**
     * Gets the execution context.
     * 
     * @return the execution context.
     */
    public ExecutionContext getContext() {
        return this.context;
    }

    /**
     * Sets the picked message handler.
     * 
     * @param handler
     * @return true if it is the first (the one picked) or false if is was not
     *         picked.
     */
    public synchronized boolean picked(OnMessageHandler handler) {
        if (this.picked != null)
            return false;

        this.picked = handler;
        this.notifyAll();
        return true;
    }
}
