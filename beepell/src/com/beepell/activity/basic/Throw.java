package com.beepell.activity.basic;

import javax.xml.namespace.QName;

import com.beepell.activity.ActivityState;
import com.beepell.exceptions.BPELFault;
import com.beepell.model.ThrowActivity;
import com.beepell.variable.Variable;

/**
 * <p>
 * The throw activity is used when a business process needs to signal an
 * internal fault explicitly. A fault MUST be identified with a QName (see
 * section 10.3. Invoking Web Service Operations). The throw activity provides
 * the name for the fault, and can optionally provide data with further
 * information about the fault. A fault handler can use such data to handle the
 * fault and to populate any fault messages that need to be sent to other
 * services.
 * <p>
 * WS-BPEL does not require fault names to be defined prior to their use in a
 * throw activity. This provides a lightweight mechanism to introduce
 * business-process faults. A fault name defined in a business process, a WSDL
 * definition or a WS-BPEL standard fault can be directly used, by using an
 * appropriate QName, as the value of the faultName attribute and providing a
 * variable with the fault data if required.
 * 
 * @author Tim Hallwyl
 */
public class Throw extends AbstractBasicActivity {
    
    private QName faultName;
    private String faultVariable;
    
    private class Fault extends BPELFault {

        /**
         * Create an unknow fault.
         * @param name
         * @param data
         */
        public Fault(QName name, Variable data) {
            super(name, data);
        }

        private static final long serialVersionUID = 1L;
        
    }
    
    /**
     * Configure the activity.
     * 
     * @param configuration
     */
    public Throw(ThrowActivity configuration) {
        super(configuration);
        this.faultName = configuration.getFaultName();
        this.faultVariable = configuration.getFaultVariable();
        setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() throws BPELFault {
        if (faultVariable != null)
            throw new Fault(faultName, context.getVariable(faultVariable).clone());
        else
            throw new Fault(faultName, null);
    }

}
