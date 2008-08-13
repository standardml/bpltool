package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: ambiguousReceive
 * </p>
 * <p>
 * Thrown when a business process instance simultaneously enables two or more
 * IMAs for the same partnerLink, portType, operation but different
 * correlationSets, and the correlations of multiple of these activities match
 * an incoming request message.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class AmbiguousReceive extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    private static final QName name = new QName(namespace, "ambiguousReceive");

    /**
     * 
     */
    public AmbiguousReceive() {
        super(name);
    }

    /**
     * @param message
     */
    public AmbiguousReceive(String message) {
        super(name, message);
    }

}
