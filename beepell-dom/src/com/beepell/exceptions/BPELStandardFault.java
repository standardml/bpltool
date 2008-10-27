package com.beepell.exceptions;

import javax.xml.namespace.QName;

import org.w3c.dom.Node;

import com.beepell.BPELConstants;

/**
 * Super type for all WS-BPEL standard faults -- needed to evaluate exist on
 * standard fault in Scopes fail method.
 * 
 * @author Tim Hallwyl
 */
public abstract class BPELStandardFault extends BPELFault {

    protected static final String namespace = BPELConstants.BPEL;
    
    /**
     * 
     * @param name the BPEL Faults qualified name.
     * @param message
     * @param cause
     */
    public BPELStandardFault(QName name, String message, Throwable cause) {
        super(name, message, cause);
    }

    /**
     * 
     * @param name the BPEL Faults qualified name.
     * @param message
     */
    public BPELStandardFault(QName name, String message) {
        super(name, message);
    }

    /**
     * 
     * @param name the BPEL Faults qualified name.
     * @param cause
     */
    public BPELStandardFault(QName name, Throwable cause) {
        super(name, cause);
    }

    /**
     * 
     * @param name the BPEL Faults qualified name.
     * @param data the fault message or element data
     */
    public BPELStandardFault(QName name, Node data) {
        super(name, data);
    }

    /**
     * 
     * @param name the BPEL Faults qualified name.
     */
    public BPELStandardFault(QName name) {
        super(name);
    }




}
