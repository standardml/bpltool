package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: uninitializedVariable
 * </p>
 * <p> Thrown when there is an attempt to access the value of an uninitialized
 * variable or in the case of a message type variable one of its uninitialized
 * parts.
 * 
 * @author Tim Hallwyl
 */
public class UninitializedVariable extends BPELStandardFault {

    private static final long serialVersionUID = 1L;

    private static final QName name = new QName(namespace, "uninitializedVariable");

    /**
     * 
     */
    public UninitializedVariable() {
        super(name);
    }

    /**
     * @param message
     */
    public UninitializedVariable(String message) {
        super(name, message);
    }

}
