package com.beepell.activity.basic;

import com.beepell.activity.ActivityState;
import com.beepell.exceptions.BPELFault;
import com.beepell.model.RethrowActivity;

/**
 * <p>
 * The rethrow activity is used in fault handlers to rethrow the fault they
 * caught, i.e. the fault name and, where present, the fault data of the
 * original fault. It can be used only within a fault handler (catch and
 * catchAll). Modifications to the fault data MUST be ignored by rethrow. For
 * example, if the logic in a fault handler modifies the fault data and then
 * call rethrow, the original fault data would be rethrown and not the modified
 * fault data. Similarly if a fault is caught using the shortcut that allows
 * message type faults with one part defined using an element to be caught by
 * fault handlers looking for the same element type, then a rethrow would
 * rethrow the original message type data (see section 12.5. Fault Handlers).
 * [10.11]
 * 
 * @author Tim Hallwyl
 */
public class Rethrow extends AbstractBasicActivity {

    /**
     * Configure the activity.
     * 
     * @param configuration
     */
    public Rethrow(RethrowActivity configuration) {
        super(configuration);
        setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() throws BPELFault {
        // TODO: Check after implementing fault handlers that this is correct.
        BPELFault fault = context.getScope().getFault();
        throw fault;
        
    }

}
