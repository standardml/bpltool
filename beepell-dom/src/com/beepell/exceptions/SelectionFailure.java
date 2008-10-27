package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: selectionFailure
 * </p>
 * <p>
 * Thrown when a selection operation performed either in a function such as
 * bpel:getVariableProperty, or in an assignment, encounters an error.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class SelectionFailure extends BPELStandardFault {

    private static final long serialVersionUID = 1L;

    private static final QName name = new QName(namespace, "selectionFailure");

    /**
     * 
     */
    public SelectionFailure() {
        super(name);
    }

    /**
     * @param message
     */
    public SelectionFailure(String message) {
        super(name, message);
    }

}
