package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: missingRequest
 * </p>
 * <p>
 * Thrown when a &lt;reply&gt; activity cannot be associated with an open
 * inbound message activity by matching the partner link, operation and message
 * exchange tuple.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class MissingRequest extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    private static final QName name = new QName(namespace, "missingRequest");

    /**
     * 
     */
    public MissingRequest() {
        super(name);
    }

    /**
     * @param message
     */
    public MissingRequest(String message) {
        super(name, message);
    }

}
