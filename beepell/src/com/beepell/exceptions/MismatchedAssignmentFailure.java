package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: mismatchedAssignmentFailure
 * </p>
 * <p>
 * Thrown when incompatible types or incompatible XML infoset structure are
 * encountered in an &lt;assign&gt; activity.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class MismatchedAssignmentFailure extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    
    private static final QName name = new QName(namespace, "mismatchedAssignmentFailure");


    /**
     * 
     */
    public MismatchedAssignmentFailure() {
        super(name);
    }

    /**
     * @param message
     */
    public MismatchedAssignmentFailure(String message) {
        super(name, message);
    }

}
