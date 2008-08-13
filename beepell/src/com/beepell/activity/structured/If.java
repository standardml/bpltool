package com.beepell.activity.structured;

import java.util.List;

import com.beepell.activity.ActivityState;
import com.beepell.exceptions.BPELFault;
import com.beepell.expression.BooleanExpression;
import com.beepell.model.Activity;
import com.beepell.model.Elseif;
import com.beepell.model.IfActivity;

/**
 * If Activity.
 * <p>
 * The if activity provides conditional behavior. The activity consists of an
 * ordered list of one or more conditional branches defined by the if and
 * optional elseif elements, followed by an optional else element.
 * 
 * @author Tim Hallwyl
 */
public class If extends AbstractStructuredActivity {

    private final BooleanExpression condition;

    private final List<Elseif> elseifs;

    private final Activity elseActivity;

    private final Activity activity;

    /**
     * Create an If Activity.
     * 
     * @param configuration
     */
    public If(IfActivity configuration) {
        super(configuration);
        this.condition = configuration.getConditionExpression();
        this.activity = configuration.getActivity();
        
        if (configuration.getElse() != null)
            this.elseActivity = configuration.getElse().getActivity();
        else
            this.elseActivity = null;
        
        this.elseifs = configuration.getElseif();

        this.setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() throws BPELFault {
        Activity selected = null;

        // Main condition is evaluated first:
        if (condition.evaluate(context)) {
            selected = activity;
        } else {
            setOutgoingLinksFalse(activity);
        }

        // If the main condition selected the main branch, then this loop will
        // set outgoing links to false for all 'elseif' activities. Otherwise,
        // they are evaluated in turn.
        if (elseifs != null) {
            for (Elseif elseif : elseifs) {
                if (getState() == ActivityState.TERMINATING)
                    return;

                if (selected != null) {
                    setOutgoingLinksFalse(elseif.getActivity());
                } else {

                    // We have not selected any activity yet.
                    if (elseif.getConditionExpression().evaluate(context)) {
                        selected = elseif.getActivity();
                    } else {
                        setOutgoingLinksFalse(elseif.getActivity());
                    }

                }

            }
        }

        // If there is an 'else' branch: if no other branch has been selected we
        // select it, otherwise we have to set its outgoing links to false.
        if (elseActivity != null) {

            if (selected == null && getState() != ActivityState.TERMINATING)
                selected = elseActivity;
            else
                setOutgoingLinksFalse(elseActivity);
        }

        // If a branch was selected, now is the time to execute it.
        if (selected != null && getState() != ActivityState.TERMINATING)
            execute(selected);

    }

}
