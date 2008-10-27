package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: invalidVariables
 * </p>
 * <p>
 * Thrown when an XML Schema validation (implicit or explicit) of a variable
 * value fails.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class InvalidVariables extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    
    private static final QName name = new QName(namespace, "invalidVariables");


    /**
     * 
     */
    public InvalidVariables() {
        super(name);
    }

    /**
     * @param message
     */
    public InvalidVariables(String message) {
        super(name, message);
    }

}
