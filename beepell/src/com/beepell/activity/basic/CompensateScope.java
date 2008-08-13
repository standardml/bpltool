package com.beepell.activity.basic;

import java.util.ArrayList;
import java.util.List;

import com.beepell.activity.ActivityState;
import com.beepell.activity.structured.Scope;
import com.beepell.exceptions.BPELFault;
import com.beepell.model.CompensateScopeActivity;

/**
 * The compensateScope activity causes one specified child scope to be
 * compensated.
 * 
 * @author Tim Hallwyl
 */
public class CompensateScope extends AbstractBasicActivity {

    private final String target;

    /**
     * @param configuration
     */
    public CompensateScope(CompensateScopeActivity configuration) {
        super(configuration);
        this.target = configuration.getTarget();
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
     * In the case of scope specific compensation (compensateScope), the
     * Compensation Handler Instance Group contains the installed compensation
     * handler instances of a particular target scope that is executed within a
     * repeatable construct.
     * <p>
     * If a scope being compensated by name is nested in a while, repeatUntil,
     * or non-parallel forEach loop, the invocation of the installed instances
     * of the compensation handlers in the successive iterations MUST be in
     * reverse order.
     * <p>
     * In the case of parallel <forEach> and event handlers, no ordering
     * requirement is imposed on the compensation of the associated scope.
     * 
     * @return All instances of the target scope.
     */
    private List<Scope> getInstanceGroup() {
        List<Scope> group = new ArrayList<Scope>();
        List<Scope> scopes = context.getScope().getImmediatelyEnclosedScopeInstances();

        for (Scope scope : scopes) {
            if (target.equals(scope.getName()) && scope.getState() == ActivityState.COMPLETED)
                group.add(scope);
        }

        return reverse(group);
    }
        
    private List<Scope> reverse(List<Scope> list) {
        List<Scope> reverse = new ArrayList<Scope>(list.size());
        for (int index = list.size() - 1; index >= 0; index--) {
            reverse.add(list.get(index));
        }
        return reverse;
    }
}
