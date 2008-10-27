package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: scopeInitializationFailure
 * </p>
 * <p>
 * Thrown if there is any problem creating any of the objects defined as part of
 * scope initialization. This fault is always caught by the parent scope of the
 * faulted scope.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class ScopeInitializationFailure extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    private static final QName name = new QName(namespace, "scopeInitializationFailure");

    /**
     * 
     */
    public ScopeInitializationFailure() {
        super(name);
    }

    /**
     * @param message
     */
    public ScopeInitializationFailure(String message) {
        super(name, message);
    }

    /**
     * @param cause
     */
    public ScopeInitializationFailure(Throwable cause) {
        super(name, cause);
    }

    /**
     * @param message
     * @param cause
     */
    public ScopeInitializationFailure(String message, Throwable cause) {
        super(name, message, cause);
    }

}
