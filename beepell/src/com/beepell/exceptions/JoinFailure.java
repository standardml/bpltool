package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: joinFailure
 * </p>
 * <p>
 * Thrown when the join condition of an activity evaluates to false and the
 * value of the suppressJoinFailure attribute is yes.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class JoinFailure extends BPELStandardFault {

    private static final long serialVersionUID = 1L;

    private static final QName name = new QName(namespace, "joinFailure");

    /**
     * 
     */
    public JoinFailure() {
        super(name);
    }

    /**
     * @param message
     */
    public JoinFailure(String message) {
        super(name, message);
    }
    
    /**
     * @param message
     * @param cause 
     */
    public JoinFailure(String message, Throwable cause) {
        super(name, message, cause);
    }

}
