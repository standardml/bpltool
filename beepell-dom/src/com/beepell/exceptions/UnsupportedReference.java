package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: unsupportedReference
 * </p>
 * <p>
 * Thrown when a WS-BPEL implementation fails to interpret the combination of
 * the reference-scheme attribute and the content element OR just the content
 * element alone.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class UnsupportedReference extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    
    private static final QName name = new QName(namespace, "unsupportedReference");

    /**
     * 
     */
    public UnsupportedReference() {
        super(name);
    }

    /**
     * @param message
     */
    public UnsupportedReference(String message) {
        super(name, message);
    }

}
