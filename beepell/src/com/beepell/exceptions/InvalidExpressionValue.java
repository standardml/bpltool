package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: invalidExpressionValue
 * </p>
 * <p>
 * Thrown when an expression used within a WS-BPEL construct (except
 * &lg;assign&gt;) returns an invalid value with respect to the expected XML
 * Schema type.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class InvalidExpressionValue extends BPELStandardFault {

    private static final long serialVersionUID = 1L;

    private static final QName name = new QName(namespace, "invalidExpressionValue");

    /**
     * 
     */
    public InvalidExpressionValue() {
        super(name);
    }

    /**
     * @param message
     */
    public InvalidExpressionValue(String message) {
        super(name, message);
    }

}
