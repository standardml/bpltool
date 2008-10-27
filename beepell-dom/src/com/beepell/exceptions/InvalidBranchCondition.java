package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: invalidBranchCondition
 * </p>
 * <p>
 * Thrown if the integer value used in the &lt;branches&gt; completion condition
 * of &lt;forEach&gt; is larger than the number of directly enclosed
 * &lt;scope&gt; activities.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class InvalidBranchCondition extends BPELStandardFault {

    private static final long serialVersionUID = 1L;

    private static final QName name = new QName(namespace, "invalidBranchCondition");

    /**
     * 
     */
    public InvalidBranchCondition() {
        super(name);
    }

    /**
     * @param message
     */
    public InvalidBranchCondition(String message) {
        super(name, message);
    }
 
}
