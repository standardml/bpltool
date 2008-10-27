package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: correlationViolation
 * </p>
 * <p>
 * Thrown when the contents of the messages that are processed in an
 * &lt;invoke&gt;, &lt;receive&gt;, &lt;reply&gt;, &lt;onMessage&gt;, or
 * &lt;onEvent&gt; do not match specified correlation information.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class CorrelationViolation extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    private static final QName name = new QName(namespace, "correlationViolation");

    /**
     * 
     */
    public CorrelationViolation() {
        super(name);
    }

    /**
     * @param message
     */
    public CorrelationViolation(String message) {
        super(name, message);
    }
    
}
