package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: subLanguageExecutionFault
 * </p>
 * <p>
 * Thrown when the execution of an expression results in an unhandled fault in
 * an expression language or query language.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class SubLanguageExecutionFault extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    private static final QName name = new QName(namespace, "SubLanguageExecutionFault");

    /**
     * 
     */
    public SubLanguageExecutionFault() {
        super(name);
    }

    /**
     * @param message
     */
    public SubLanguageExecutionFault(String message) {
        super(name, message);
    }

    /**
     * @param cause
     */
    public SubLanguageExecutionFault(Throwable cause) {
        super(name, cause);
    }

    /**
     * @param message
     * @param cause
     */
    public SubLanguageExecutionFault(String message, Throwable cause) {
        super(name, message, cause);
    }

}
