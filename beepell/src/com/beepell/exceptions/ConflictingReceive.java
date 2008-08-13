package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: conflictingReceive
 * </p>
 * <p>
 * Thrown when more than one inbound message activity is enabled simultaneously
 * for the same partner link, port type, operation and correlation set(s).
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class ConflictingReceive extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    private static final QName name = new QName(namespace, "conflictingReceive");
    
    /**
     * 
     */
    public ConflictingReceive() {
        super(name);
    }

    /**
     * @param message
     */
    public ConflictingReceive(String message) {
        super(name, message);
    }


}
