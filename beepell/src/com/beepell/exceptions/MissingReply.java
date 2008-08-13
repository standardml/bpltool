package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: missingReply
 * </p>
 * <p>
 * Thrown when an inbound message activity has been executed, and the process
 * instance or scope instance reaches the end of its execution without a
 * corresponding &lt;reply&gt; activity having been executed.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class MissingReply extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    private static final QName name = new QName(namespace, "missingReply");

    /**
     * 
     */
    public MissingReply() {
        super(name);
    }

    /**
     * @param message
     */
    public MissingReply(String message) {
        super(name, message);
    }

}
