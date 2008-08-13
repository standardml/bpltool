package com.beepell.activity.basic;

import java.util.ArrayList;
import java.util.List;

import com.beepell.activity.ActivityState;
import com.beepell.activity.structured.Scope;
import com.beepell.exceptions.BPELFault;
import com.beepell.model.CompensateActivity;

/**
 * The compensate activity causes all immediately enclosed scopes to be
 * compensated in default order (see section 12.5.2. Default Compensation
 * Order).
 * <p>
 * This activity is used when an FCT-handler needs to perform additional work,
 * such as updating variables, in addition to performing default compensation
 * for the targeted immediately enclosed scopes.
 * 
 * @author Tim Hallwyl
 */
public class Compensate extends AbstractBasicActivity {

    /**
     * Create a compensate activity.
     * 
     * @param configuration
     */
    public Compensate(CompensateActivity configuration) {
        super(configuration);
        setState(ActivityState.READY);

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.activity.AbstractActivity#run()
     */
    @Override
    protected void run() throws BPELFault {
        List<Scope> instances = getInstanceGroup();
        for (Scope scope : instances) {
            if (getState() != ActivityState.TERMINATING) {
                scope.compensate();
                if (scope.getState() == ActivityState.FAILED) {
                    // 1. uninstall compensation handlers
                    for (Scope scope2 : instances)
                        scope2.uninstallCompensationHandler();

                    // 2. rethrow fault
                    throw scope.getFault();

                }
            } else
                scope.uninstallCompensationHandler();
        }
    }

    /**
     * Gets a list of Scope activities in the Compensation Handler Instance
     * Group.
     * <p>
     * For the case of default compensation (the compensate activity), the
     * Compensation Handler Instance Group contains the compensation handler
     * instances of all immediately enclosed scopes that completed successfully.
     * [12.4.4.1]
     * 
     * @return List of Scope activities in the Compensation Handler Instance
     *         Group
     */
    private List<Scope> getInstanceGroup() {
        List<Scope> group = new ArrayList<Scope>();
        List<Scope> scopes = context.getScope().getImmediatelyEnclosedScopeInstances();

        for (Scope scope : scopes) {
            if (scope.getState() == ActivityState.COMPLETED)
                group.add(scope);
        }

        return group;
    }


}
