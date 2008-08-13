package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: conflictingRequest
 * </p>
 * <p>
 * Thrown when more than one inbound message activity is open for the same
 * partner link, operation and message exchange.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class ConflictingRequest extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    private static final QName name = new QName(namespace, "conflictingRequest");

    /**
     * 
     */
    public ConflictingRequest() {
        super(name);
    }

    /**
     * @param message
     */
    public ConflictingRequest(String message) {
        super(name, message);
    }


}
